# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: Read data from Human Mortality Database. construct period and cohort sex ratio curve and crossover objects. Write them to disk.
# Required packages: data.table, pbapply
#  The functions that start with 'hmd' were adapted from code by Hyndman et al. in the package 'demography'


# Load packages
library(data.table) # data manipulation functions
library(pbapply) # progress bar for the 'lapply()' function

# Reads in the HMD codes. Note, these were adapted from the Hyndman's CRAN demography package. My require further updating if HMD has since altered the codes.
ccodes <- read.csv('./HMD Country Codes.csv', stringsAsFactors = FALSE)

# Convenience functions
# A convenience function to rbind a list using the data.table package, but cast it back into data.frame format.
rBindThisList <- function(theList){
  as.data.frame(rbindlist(theList))
}
# Converts "factor" variables into strings
deLevel <- function(v){
  levels(v)[v]
}

# Get the single-year period life tables from HMD for a given sex (male, female, or both). Can be one, two, or all three. Requires HMD username and password
# Throws an error if there is a problem reading file (example, username or password invalid, bad country name, wrong input for sex specifications, changed file format, etc...)
#  Additionally, the minimum age required for cohort life tables can be overridden (the default is 50)
hmd.lifetab <- function(country, sex=c('m','f','b'), username, password, ltType='period', minAgeRequiredForCohortLifeTabs=50) 
{
  # Check for valid inputs
  if (!(sex %in% c('m','f','b'))){
    stop('sex must be m, f, or b')
  } 
  if (!(ltType %in% c('period','cohort'))){
    stop('ltType must be period or cohort')
  }
  
  # Connect to HMD and read data
  # Set the username and password for HMD as one string
  userpwd <- paste(username, ":", password, sep = "")
  
  # These three lines set the url, including username and password
  taburl0 <- paste(sex, 'ltper_1x1.txt', sep='') 
  path <- paste('http://www.mortality.org/hmd/', country, '/STATS/', 
                taburl0, sep = '') # The HMD URL to connect to
  txt <- RCurl::getURL(path, userpwd = userpwd)
  # Read the life table data from HMD
  con <- textConnection(txt) 
  lifetab <- try(utils::read.table(con, skip = 2, header = TRUE, 
                                   na.strings = "."), TRUE)
  close(con)
  if (class(lifetab) == "try-error") 
    stop("Connection error at www.mortality.org. Please check username, password and country label.")
  # Store the country and sex as variable names in an R data.frame
  lifetab$country <- country
  lifetab$sex <- sex
  # If the type of life table is period, we're done. Simply return the life tables.
  if (ltType=='period'){
    return(lifetab)
  }
  
  # If the code reaches this point, then cohort life tables were requested
  # Set up the cohort life tables.  In order to do this, we need to merge the data for age x in year c + x with age x in year c. 
  #   First, get a subset of the data, set the reference point, and then merge.
  clt <- lifetab[c(1,2,11,12)]
  # Get the cohort reference year (which is year plus age x)
  clt$age <- as.numeric(substr(clt$Age, 1, 3))
  clt$refYear <- clt$Year + clt$age
  colnames(lifetab)[1] <- 'refYear'
  # Merge them together
  clt2 <- merge(clt, lifetab[,c('refYear','Age','qx')])
  clt2 <- clt2[order(clt2$sex, clt2$Year, clt2$age),]
  # Generate the lx values using a conventional method
  
  # Get life table l(x) from q(x). This is a discrete lifetable function. It might be updated to use a different method as desired.
  # This function is designed to work with the parameter data.frame 'dat', which is assumed to be 'clt' above. Out of context, it might not work.
  lxFromQx <- function(dat){
    # Initial fields
    qx <- dat$qx
    # The last qx value
    topI <- length(qx)
    # Set up life table vectors
    dx <- lx <- qx*0-9
    # lx radix of 100000
    lx[1] <- 100000
    
    # Generate the life table values d(x) and l(x)
    # (Note this code contains looping in R, and could probably be recoded to improve performance)
    for (i in 1:(topI-1)){
      dx[i] <- lx[i]*qx[i]
      lx[i+1] <- lx[i] - dx[i]
    }
    # Return a data.frame including the country, sex, year, age, qx, and lx
    data.frame(country=dat$country, sex=dat$sex, Year=dat$Year, Age=dat$Age, age=dat$age, qx=dat$qx, lx)
  }
  
  # For each year, get the lx values using the function above.
  # Require at least the minimum number of data points set above by the user.
  clt3List <- lapply(split(clt2, clt2$Year), function(x0){
    # If the number of rows is deficient for a cohort life table (as set in the parameter above), return NA
    if (nrow(x0) <= minAgeRequiredForCohortLifeTabs){
      return(NA)
    }
    return(lxFromQx(x0))
  })
  # append all the life tables into one giant long-form data.frame
  clt4 <- rBindThisList(clt3List[!is.na(clt3List)])
  
  # Must be at least one year available for analysis
  if (nrow(clt4) == 0){
    return(NA)
  }
  # Done
  return(clt4)
}

# This function get the secondary sex ratios from births. 
#  It also includes the births (by sex) in the output.
#   It requires the country, and HMD username and password
hmd.countrySSRs <- function(country,  username, password){
  # Set the path, and connect to the database
  taburl <- 'Births.txt'  # paste(sex, "ltper_1x1.txt", sep='')
  path <- paste("http://www.mortality.org/hmd/", country, "/STATS/", 
                taburl, sep = "")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- RCurl::getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  # Read the data
  births <- try(utils::read.table(con, skip = 2, header = TRUE, 
                                  na.strings = "."), TRUE)
  close(con)
  if (class(births) == "try-error") 
    stop("Connection error at www.mortality.org. Please check username, password and country label.")
  # Generate columns for the secondary sex ratio (i.e., at birth) and the country
  births$SSR <- births$Male/births$Female
  births$country <- country
  # Return the data.frame
  births
}

# This function does the sex ratio analyses.
#  Requires year-specific life-table data, formatted like the HMD lifetables read in from the hmd.periodLifeTab function above, and passed to the function as 'datList' 
# It must be provided as the first item of a list in "datList". 
#  Either datList must have a second item that is a dataset of SSRs, with columns including 'Year' and 'SSR'; or, an override constant SSR must be provided (other than the default of NULL).
# A 'lowerBound' parameter can be set to determine the first chronilogical secular year of data. It defaults to 0 (i.e., show all years)
# Output includes:
#  type: the type of life table (period or cohort)
#  country - the country being analyzed
#  SSR - the secondary sex ratio (males / females at birth)
#  srxDat - a dataset with sex ratio crossover (SRX) for each year
#  origDat - The original data passed in, order by year and age
#  
srAnalyses <- function(datList, lowerBoundYear=1850, SSR=NA, type='period'){
  if (class(datList) != 'list'){
    stop('datList must have class list')
  }
  if (!(type %in% c('period','cohort'))){
    stop('type must be period or cohort')
  }
  
  # Keep only the country, Year, Age, and lx variables
  colsToKeep <- c('country','Year','Age','lx')
  # The life table data
  ltdat <- datList[[1]]
  # The SSR data, if available.
  ssrDat <- NULL
  if (length(datList) > 1){
    ssrDat <- datList[[2]]  
  }
  
  # Get the male and female data into a wideform (this includes separating datasets and then merging)
  f <- ltdat[ltdat$sex=='f', colsToKeep]
  colnames(f)[4] <- 'lx.f'
  m <- ltdat[ltdat$sex=='m', colsToKeep]
  colnames(m)[4] <- 'lx.m'
  d0 <- merge(f, m)
  
  # For each year in which a lifetable is available
  ltYears <- unique(ltdat$Year)
  
  # If a SSR override was provided, use it.
  if (!is.na(SSR)){
    ssrDat <- data.frame(Year=ltYears, SSR)
  }
  
  # Merge in the secondary sex ratio for each row
  d <- merge(d0, ssrDat[,c('Year','SSR')], all.x=TRUE)
  
  # Calculate the sex ratio for each row (i.e., by year and age)
  d$lx.sr <- d$SSR * d$lx.m / d$lx.f
  # Get "Age" into numeric format
  d$age <- as.numeric(deLevel(d$Age))
  # Keep only up through age 100
  d1 <- d[!(is.na(d$age)) & d$age <= 100,]
  # Store these data as "origDat"
  origDat <- d1
  # Get the country and country name
  country <- d1$country[1]
  cName <- ccodes$country[ccodes$code==country]
  
  # Get only the data since the lower bound year, and also get the "Year" variable into character format for plotting
  temp <- d1[d1$Year >= lowerBoundYear,]
  temp$year <- as.character(temp$Year)
  
  # For each year, this finds the first (minimum) age at which the sex ratio dips below 1
  crossAgex <- as.data.frame(rbindlist(lapply(split(d1, d1$Year), function(x0){
    # Order the data points by age
    x <- x0[order(x0$age),]
    # Find every place the sex ratio curve is less than 1.
    srcNotGreaterOne <- (x$lx.sr < 1)
    
    # Sometimes, there might be life table data, but without the births recorded (this occured with Chile during testing)
    #  If everything matches, then it's "okToAnalyze"
    okToAnalyze <- !any(is.na(c(x$lx.f, x$lx.m, x$SSR, x$lx.sr)))
    
    # A variable to store the sex ratio crossover (i.e., srx)
    srx <- NA
    if (okToAnalyze){
      if (any(srcNotGreaterOne)){
        srx <- min(x$age[srcNotGreaterOne], na.rm=TRUE)-1 # The sex ratio crossover is the minimum index of the age where the sex ratio curve (SRC) is less than 1, minus one (since we include age 0)
      }
    }
    return(data.frame(year=x$Year[1], srx))
  })))
  srxDat <- crossAgex[!is.na(crossAgex$srx) & crossAgex$year >= lowerBoundYear,]
  srxDat$country <- country
  
  # Construct and then return objects of interest
  obj <- list(type, country, SSR, srxDat, origDat[order(origDat$Year, origDat$age),])
  names(obj) <- c('type', 'country', 'SSR', 'srxDat', 'origDat')
  return(obj)
}


# A wrapper function that gets the sigle-year life tables for a given country for males, females, and both sexes. Calls the hmd.periodLifeTab function, and might throw associated errors.
#  Requires country, username, password, and type of life table (period or cohort). Default life table type is period.
getCountryLifeTabs <- function(country,  username, password, ltType='period'){
  # Read the female, male, and "both" life tables. 
  f <- hmd.lifetab(country, 'f', username, password, ltType)
  m <- hmd.lifetab(country, 'm', username, password, ltType)
  b <- hmd.lifetab(country, 'b', username, password, ltType)
  # Append all the life tables into one data.frame, and return it.
  rbind(f,m,b)
}


# A wrapper function that will set up data for a sex ratio analysis, using either the period or cohort measure
#  Output is stored for each country
#   For each country:
#    It will read the lifetables for males, females, and both, and store them in the 'lifetab' object.
#    It will also read the secondary sex ratios, and store them in the 'SSRs' object
#  The function requires a list of countries to examine (formatted by HMD acronyms), HMD username and password, and the type of life table (period or cohort)
setupSrData <- function(countriesToExamine, username=hmd.password, password=hmd.password, ltType='period'){
  # Read data from HMD, and format
  allDat <- pblapply(countriesToExamine, function(x0) {
    print(x0)
    lifetabs <- getCountryLifeTabs(x0, username=hmd.username, password=hmd.password, ltType)
    if (is.na(lifetabs)){
      return(NA)
    }
    SSRs <- hmd.countrySSRs(x0, username=hmd.username, password=hmd.password)
    obj <- list(lifetabs, SSRs)
    names(obj) <- c('lifetabs', 'SSRs')
    obj
  })
  # Format name the data by country
  names(allDat) <- countriesToExamine
  # Check to see if any of the countries did not yield sufficient data for analysis (this happens when no cohort data are available for a country, as defined by the user in the 'minAgeRequiredForCohortLifeTabs' parameter above
  if (any(is.na(allDat))){
    missingCNames <- names(allDat)[is.na(allDat)]
    warning(paste('Insufficient data to analyze', missingCNames, 'using', ltType, 'life tables'))
  }
  
  populatedDat <- allDat[!is.na(allDat)]
}
# Set up period and cohort data, including life tables and SSRs
print('Reading Period Data from HMD')
perdat <- setupSrData(countriesToExamine, ltType='period') 
print('Reading Cohort Data from HMD')
cohdat <- setupSrData(countriesToExamine, ltType='cohort') 

# Run analyses, and generate the sex ratio analyses objects
perFits <- lapply(perdat, srAnalyses, type='period', lowerBoundYear=earliestAnalysisYear)
cohFits <- lapply(cohdat, srAnalyses, type='cohort', lowerBoundYear=earliestAnalysisYear)

# The HMD requires a login. The SRX objects contain some raw data. So, they cannot be stored in a public GIT Repository. Reference a personal folder on local machine.
# It's good to include the date in the filename, because HMD updates over time. This will yield a fixed file referencing the date;
objectFileName <- paste('PeriodCohortSRXObjects', Sys.Date(), sep='_')
save(list=c('perdat','cohdat','perFits','cohFits'), file=paste(personalFolder, objectFileName, sep=''))

