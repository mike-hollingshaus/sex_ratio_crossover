# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: Period and Cohort Sex Ratio Crossover Ages throughout History

library(data.table)
library(ggplot2)
library(demography)
library(pbapply)

# Empty out memory
rm(list=ls())

# A prefrence function to clear the console
cls <- function() cat('\014')
cls()

# A convenience function to rbind a list using the data.table package, but cast it back into data.frame format
rBindThisList <- function(theList){
  as.data.frame(rbindlist(theList))
}
# Set the working directory to the local repository
setwd('D:/Current Projects/Mortality Sex Ratio Changes/sex_ratio_crossover')
# Load in the color schemes
source('color schemes.R')

# A file on the local harddrive for reading in the author's username/password for HMD. In practice, comment these out, and hardcode the user's username and password with literal strings. 
hmd.loginFile <- 'C:/Users/u0214256/Documents/Keys/human_mortality_database.csv'  
hmd.loginData <- trimws(as.character(read.csv(hmd.loginFile, stringsAsFactors = FALSE)[1,]))
# Username and password for HMD. Replace these with the individual user's literal strings.
hmd.username <- hmd.loginData[1]
hmd.password <- hmd.loginData[2]
minAgeRequiredForCohortLifeTabs <- 50


# Reads in the HMD codes. Note, these were adapted from the Hyndman's CRAN demography package. My require further updating if HMD has since altered the codes.
ccodes <- read.csv('./HMD Country Codes.csv', stringsAsFactors = FALSE)

# Rather than examine all countries, choose only select examples from around the world
countriesToExamine <- c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL')

per_coh_colours <- c(sr_cols$primary$`Masters Black`, sr_cols$primary$Red)

# A Map for country colors
country_color_map <- 
  c(
    sr_cols$tertiary$`Amber Yellow`,
    sr_cols$tertiary$`Amazon Green`,
    sr_cols$tertiary$`Azure Blue`,
    sr_cols$primary$`Masters Black`,
    sr_cols$secondary$`Wood`,
    sr_cols$primary$`Granite Grey`,
    sr_cols$primary$`Red`
  )
names(country_color_map) <- countriesToExamine[order(countriesToExamine)]

# This line of code will examine all hmd countries: countriesToExamine <- ccodes$code

# A handy function to get factor "levels" into strings.
deLevel <- function(v){
  levels(v)[v]
}

# Get e0 from qx. This is a discrete lifetable function. It might be updated to use a different method as desired.
#  qx and agex are vectors for the respective lifetable parameters.
lxFromQx <- function(dat){
  qx <- dat$qx
  # Initial fields
  # The last qx value
  topI <- length(qx)
  # Set up life table vectors
  dx <- lx <- qx*0-9
  # lx radix of 100000
  lx[1] <- 100000
  
  # The lifetable values, including conventional weights.
  ### Note this code contains looping in R, and could probably be recoded to improve performances
  for (i in 1:(topI-1)){
    dx[i] <- lx[i]*qx[i]
    lx[i+1] <- lx[i] - dx[i]
  }
  data.frame(country=dat$country, sex=dat$sex, Year=dat$Year, Age=dat$Age, age=dat$age, qx=dat$qx, lx)
}

# Get the single-year period life tables from HMD for a given sex (male, female, or both). Can be one, two, or all three. Requires HMD username and password
# Throws an error if there is a problem reading file (example, username or password invalid, bad country name, wrong input for sex specifications, changed file format, etc...)
hmd.lifetab <- function(country, sex=c('m','f','b'), username, password, ltType='period', label = country) 
{
  if (!(sex %in% c('m','f','b'))){
    stop('sex must be m, f, or b')
  }
  if (!(ltType %in% c('period','cohort'))){
    stop('ltType must be period or cohort')
  }
  
  taburl <- paste(sex, "ltper_1x1.txt", sep='')
  path <- paste("http://www.mortality.org/hmd/", country, "/STATS/", 
                taburl, sep = "")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- RCurl::getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  lifetab <- try(utils::read.table(con, skip = 2, header = TRUE, 
                                   na.strings = "."), TRUE)
  close(con)
  if (class(lifetab) == "try-error") 
    stop("Connection error at www.mortality.org. Please check username, password and country label.")
  lifetab$country <- country
  lifetab$sex <- sex
  if (ltType=='period'){
    return(lifetab)
  }
  # The cohort lifetable
  # Set it up
  clt <- lifetab[c(1,2,11,12)]
  # Get the cohort reference year (which is year plus age x)
  clt$age <- as.numeric(substr(clt$Age, 1, 3))
  clt$refYear <- clt$Year + clt$age
  colnames(lifetab)[1] <- 'refYear'
  # Merge them together
  clt2 <- merge(clt, lifetab[,c('refYear','Age','qx')])
  clt2 <- clt2[order(clt2$sex, clt2$Year, clt2$age),]
  # Generate the lx values using a conventional method
  # for each year, get the lx values
  # Require at least 30 data points
  clt3List <- lapply(split(clt2, clt2$Year), function(x0){
    # If the number of rows is deficient for a cohort life table (as set in the parameter above), return NA
    if (nrow(x0) <= minAgeRequiredForCohortLifeTabs){
      return(NA)
    }
    return(lxFromQx(x0))
  })
  # rBindThisList()
  clt4 <- rBindThisList(clt3List[!is.na(clt3List)])
  
  # Must be at least one year available for analysis
  
  if (nrow(clt4) ==0){
    return(NA)
  }
  # Done
  return(clt4)
}

# Get the sigle-year period life tables for a given country. Calls the hmd.periodLifeTab function, and might throw associated errors.
hmd.countryLifeTabs <- function(country,  username, password, ltType='period'){
  f <- hmd.lifetab(country, 'f', username, password, ltType)
  m <- hmd.lifetab(country, 'm', username, password, ltType)
  b <- hmd.lifetab(country, 'b', username, password, ltType)
  rbind(f,m,b)
}

# Get the secondary sex ratios from births. Also, include the births (by sex) in the output
hmd.countrySSRs <- function(country,  username, password){
  taburl <- 'Births.txt'  # paste(sex, "ltper_1x1.txt", sep='')
  path <- paste("http://www.mortality.org/hmd/", country, "/STATS/", 
                taburl, sep = "")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- RCurl::getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  births <- try(utils::read.table(con, skip = 2, header = TRUE, 
                                   na.strings = "."), TRUE)
  close(con)
  if (class(births) == "try-error") 
    stop("Connection error at www.mortality.org. Please check username, password and country label.")
  births$SSR <- births$Male/births$Female
  births$country <- country
  births
}

# This function does the first sex ratio analyses for a given set of year-specific life-table data, formatted like the HMD lifetables read in from the hmd.periodLifeTab function. (Note, this requires a country name field). It must be provided as the first item of a list in "datList". 
#  Either datList must have a second item that is a dataset of SSRs, with columns including 'Year' and 'SSR'; or, an override constant SSR must be provided (other than the default of NULL).
# The lowerBound parameter determines the first year for which the regression analyses begin.
# The first 
# Output includes: STILL WORKING ON THIS
srAnalyses <- function(datList, lowerBoundYear=0, SSR=NA, type='period'){
  if (class(datList) != 'list'){
    stop('datList must have class list')
  }
  if (!(type %in% c('period','cohort'))){
    stop('type must be period or cohort')
  }
  
  # Keep only the country, Year, Age, and lx variables
  colsToKeep <- c('country','Year','Age','lx')
  
  
  ltdat <- datList[[1]]
  
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
  print(paste(type, 'Sex Ratio Analysis', cName))
  
  # Get only the data since the lower bound year, and also get the "Year" variable into character format for plotting
  temp <- d1[d1$Year >= lowerBoundYear,]
  temp$year <- as.character(temp$Year)
  
  # Plots of select sex ratio curves since the lower bound year
  summaryCurve <- ggplot(temp[temp$Year %% 20 == 10 | temp$Year == 2000,], aes(x=age, y=lx.sr, colour=year)) + geom_line() + geom_hline(yintercept = 1, linetype=3) + labs(title=paste(type, 'Sex Ratio Curves\n',cName, 'Select Years'))
  
  # Plots of all individual sex ratio curves.
  allCurves <- lapply(split(temp, temp$Year), function(x0){
    ggplot(x0, aes(x=age, y=lx.sr)) + geom_line() + geom_hline(yintercept = 1, linetype=3) + labs(title=paste('Sex Ratio Curve\n',cName, x0$Year[1])) 
  })
  
  # For each year, this finds the first (minimum) age at which the sex ratio dips below 1
  crossAgex <- as.data.frame(rbindlist(lapply(split(d1, d1$Year), function(x0){
    # Order the data points by age
    x <- x0[order(x0$age),]
    # Find every place the sex ratio curve is less than 1.
    srcNotGreaterOne <- (x$lx.sr < 1)
  
    # Sometimes, this never occurs. In such cases, the recommended action is...
   
    ageCross <- NA
    if (any(srcNotGreaterOne)){
      ageCross <- min(x$age[srcNotGreaterOne], na.rm=TRUE)-1  
    }

    # Among the ages where this is happening, the minimum age - 1 is the sex ratio crossover age
    return(data.frame(year=x$Year[1], ageCross))
  })))
  crossAgex$country <- country
  allCrossPoints <- crossAgex
  
  # Get only that subset since the lower bound year
  crossAgex <- allCrossPoints[allCrossPoints$year >= lowerBoundYear,]

  # Plots
  
  basePlot <- ggplot(allCrossPoints, aes(x=year, y=ageCross)) + geom_point() + scale_y_continuous(limits=c(0,70)) + labs(title=paste('Sex Ratio Crossover by Year', type, 'Life Tables\n',cName)) # all crossover points for the country
  # linPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=linFit)) + labs(title=paste('Sex Ratio Crossover Linear Fit', type, 'Life Tables\n',cName)) # Points (with linear fit) since the lowerBound year
  # expPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=expFit)) + labs(title=paste('Sex Ratio Crossover Exponential Fit', type,  'Life Tables\n',cName))
  
  # Return objects of interest
  obj <- list(type, country, SSR, crossAgex, basePlot, allCurves, summaryCurve, origDat[order(origDat$Year, origDat$age),], allCrossPoints)
  names(obj) <- c('type', 'country', 'SSR', 'crossAgex', 'basePlot', 'allCurves', 'summaryCurve', 'origDat', 'allCrossPoints')
  ### Again, comment out the version that fits the exponential model
  # obj <- list(country, SSR, crossAgex, fitParms, linReg, expReg, basePlot, linPlot, expPlot, allCurves, summaryCurve, origDat)
  # names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'expReg', 'basePlot', 'linPlot', 'expPlot', 'allCurves', 'summaryCurve', 'origDat')
  obj
}

# This function will set up data for a sex ratio analysis, using either the period or cohort measure
setupSrData <- function(countriesToExamine, username=hmd.password, password=hmd.password, ltType='period'){
  allDat <- pblapply(countriesToExamine, function(x0) {
    print(x0)
    lifetabs <- hmd.countryLifeTabs(x0, username=hmd.username, password=hmd.password, ltType)
    if (is.na(lifetabs)){
      return(NA)
    }
    SSRs <- hmd.countrySSRs(x0, username=hmd.username, password=hmd.password)
    obj <- list(lifetabs, SSRs)
    names(obj) <- c('lifetabs', 'SSRs')
    obj
  })
  names(allDat) <- countriesToExamine
  if (any(is.na(allDat))){
    missingCNames <- names(allDat)[is.na(allDat)]
    warning(paste('Insufficient data to analyze', missingCNames, 'using', ltType, 'life tables'))
  }
  populatedDat <- allDat[!is.na(allDat)]
}
print('Reading Period Data from HMD')
perdat <- setupSrData(countriesToExamine, ltType='period') 
print('Reading Cohort Data from HMD')
cohdat <- setupSrData(countriesToExamine, ltType='cohort') 

startYear <- 1850

perFits <- lapply(perdat, srAnalyses, type='period', lowerBoundYear=startYear)
cohFits <- lapply(cohdat, srAnalyses, type='cohort', lowerBoundYear=startYear)

# Figure 1 - Period Sex Ratio Curve for U.S. 2010 period lifetable
perFits$USA$allCurves$`2010` + labs(title='') + ylab(label='Sex Ratio') + xlab(label='Age') + scale_y_continuous(limits=c(0.4,1.1), breaks=seq(0.2,1.2,.2))

# A figure showing the excess male mortality, and cumulative excess
# Approximate mu_x with m_x
usa2010 <- perdat$USA$lifetabs[perdat$USA$lifetabs$Year==2010,c('sex','Age','mx')]

u10m <- usa2010[usa2010$sex=='m',]
u10f <- usa2010[usa2010$sex=='f',]
colnames(u10m)[3] <- 'm.mx'
colnames(u10f)[3] <- 'f.mx'
usa2010.2 <- merge(u10m[,c('Age','m.mx')], u10f[,c('Age','f.mx')])
# Get numeric age, and order data
usa2010.2$age <- as.numeric(deLevel(usa2010.2$Age))
usa2010.2 <- usa2010.2[order(usa2010.2$age),]

# Plot the male vs. female mx
plotDat <- melt(usa2010.2[,c('age','m.mx','f.mx')], 'age')
plotDat2 <- plotDat[plotDat$age <= 60 & !is.na(plotDat$age),]
plotDat2$variable <- deLevel(plotDat2$variable)
plotDat2$Sex <- ifelse(plotDat2$variable=='m.mx', 'Male', 'Female')

ggplot(plotDat2, aes(x=age, y=value, colour=Sex)) + geom_line() + ylab(label='Mortality Rate mx') + xlab(label='Age') + scale_colour_manual(values=per_coh_colours)  # +geom_ribbon(data=usa2010.2[usa2010.2$age <= 50 & !is.na(usa2010.2$age),], aes(x=age, ymin=f.mx, ymax=m.mx), inherit.aes=FALSE, fill = "grey70")

# Calculate the excess and cumulative excess male mortality
usa2010.2$dmr  <- usa2010.2$m.mx-usa2010.2$f.mx
usa2010.2$cdmr <- cumsum(usa2010.2$dmr)
plotDat <- melt(usa2010.2[usa2010.2$age <= 60,c('age','dmr','cdmr')], 'age')
plotDat$variable <- deLevel(plotDat$variable)
plotDat$Variable <- ifelse(plotDat$variable=='dmr', 'DMR', 'Cumulative DMR')

# Get the log secondary sex ratio. 
us10SSR <- perdat$USA$SSRs$SSR[perdat$USA$SSRs$Year==2010]
log_us10SSR <- log(us10SSR)

ggplot(plotDat, aes(x=age, y=value, colour=Variable)) + geom_line() + geom_hline(yintercept=log_us10SSR, linetype=3) + ylab(label='Male mx less Female mx') + xlab(label='Age') + scale_colour_manual(values=per_coh_colours)

perFullCrossDat <- rBindThisList(lapply(perFits, function(x0) x0$allCrossPoints))
cohFullCrossDat <- rBindThisList(lapply(cohFits, function(x0) x0$allCrossPoints))

perPlotDat <- perFullCrossDat[perFullCrossDat$year >= startYear,]
cohPlotDat <- cohFullCrossDat[cohFullCrossDat$year >= startYear,]

plotCountries <- function(plotDat){
  legNames <- unique(perPlotDat$country)[order(unique(perPlotDat$country))]
  ggplot(plotDat, aes(x=year, y=ageCross, colour=country)) + geom_jitter() + xlab(label='Year') + ylab(label = 'Sex Ratio Crossover') + labs(title='Period Sex Ratio Crossovers,\nSelect Countries') + scale_colour_manual(values=country_color_map[legNames])
}
plotCountries(perPlotDat)
plotCountries(cohPlotDat)

plotCountryPeriodCohortComparison <- function(perDat, cohDat, shortCName, plotCName, plotYLim=c(0,100), plotXLim=c(1850,2016)){
  perDat$type <- 'Period'  
  cohDat$type <- 'Cohort'
  countryPerDat <- perDat[perDat$country==shortCName,]
  countryCohDat <- cohDat[cohDat$country==shortCName,]
  
  ggplot(rbind(countryPerDat, countryCohDat), aes(x=year, y=ageCross, colour=type)) + geom_jitter() + # Main Plot
    scale_x_continuous(limits=plotXLim) + scale_y_continuous(limits=plotYLim) + # Scales  
    xlab(label='Year') + ylab(label = 'Sex Ratio Crossover') + labs(title=paste('Period vs. Cohort Sex Ratio Crossovers\n', plotCName, sep='')) + # Labels / Title                                                                  
    scale_colour_manual(values=per_coh_colours)
}

plotCountryPeriodCohortComparison(perPlotDat, cohPlotDat, 'ITA', 'Italy')
plotCountryPeriodCohortComparison(perPlotDat, cohPlotDat, 'SWE', 'Sweden')
plotCountryPeriodCohortComparison(perPlotDat, cohPlotDat, 'USA', 'United States')
plotCountryPeriodCohortComparison(perPlotDat, cohPlotDat, 'JPN', 'Japan')
plotCountryPeriodCohortComparison(perPlotDat, cohPlotDat, 'RUS', 'Russia')

# Basic descriptive statistics for SSRs
allSSRs <- rBindThisList(lapply(perdat, function(x0) x0$SSRs))
allSSRs <- allSSRs[allSSRs$Year >= 1850,]
yearRangeList <- lapply(split(allSSRs, allSSRs$country), function(x0) range(x0$Year))
cname <- names(yearRangeList)
# Store in a table
ssrDat <- data.frame(country=cname, matrix(unlist(yearRangeList), nrow=7, byrow=T))
colnames(ssrDat)[2:3] <- c('minyear','maxyear')
ssrDat$yearRange <- paste(ssrDat$minyear, ssrDat$maxyear, sep='-')
ssrDat$minSSR <-  unlist(lapply(split(allSSRs, allSSRs$country), function(x0) min(x0$SSR)))
ssrDat$maxSSR <-  unlist(lapply(split(allSSRs, allSSRs$country), function(x0) max(x0$SSR)))
ssrDat$meanSSR <- unlist(lapply(split(allSSRs, allSSRs$country), function(x0) mean(x0$SSR)))
ssrDat$stdev_100 <- unlist(lapply(split(allSSRs, allSSRs$country), function(x0) sd(x0$SSR)*100))
ssrDat$pearsons <-unlist(lapply(split(allSSRs, allSSRs$country), function(x0) cor(x0$Year, x0$SSR)))
ssrDat <- ssrDat[,c('country','yearRange','minSSR','maxSSR','meanSSR','stdev_100','pearsons')]
# Extend to the full sample
allSSRDat <- ssrDat[1,]; allSSRDat$country <- 'All'
allSSRDat$yearRange <- paste(min(allSSRs$Year), max(allSSRs$Year), sep='-')
allSSRDat$minSSR <- min(allSSRs$SSR); allSSRDat$maxSSR <- max(allSSRs$SSR); allSSRDat$meanSSR <- mean(allSSRs$SSR)
allSSRDat$stdev_100 <- sd(allSSRs$SSR)*100; allSSRDat$pearsons <- cor(allSSRs$Year, allSSRs$SSR)
# Append them together
print(ssrTab <- rbind(allSSRDat, ssrDat))
hist(allSSRs$SSR, xlab='Secondary Sex Ratio', main='')


# Plot the secondary ratios for each country
print(ssrGrandMean <- mean(allSSRs$SSR))
ggplot(allSSRs, aes(x=Year, y=SSR)) + geom_point() + geom_hline(yintercept = ssrGrandMean, linetype=3) + scale_y_continuous(limits=c(1,1.1)) + ylab(label='Secondary Sex Ratio')
print(summary(allSSRs$SSR))

# # Each country individually

# lapply(split(cohFullCrossDat, cohFullCrossDat$country), function(x0){
#   ggplot(x0, aes(x=year, y=ageCross)) + geom_point() + xlab(label='year') + ylab(label = 'Sex Ratio Crossover') + labs(title=paste(x0$country[1], 'Cohort Sex Ratio Crossover')) 
# })
# ggplot(plotDat, aes(x=year, y=ageCross, colour=country)) + geom_jitter() + xlab(label='Year') + ylab(label = 'Sex Ratio Crossover') + labs(title='Period Sex Ratio Crossovers,\nSelect Countries') + scale_colour_manual(values=country_color_map[legNames])

