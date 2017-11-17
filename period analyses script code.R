# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: Period Sex Ratio Crossover Ages throughout History

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

# A file on the local harddrive for reading in the author's username/password for HMD. In practice, comment these out, and hardcode the user's username and password with literal strings. 
hmd.loginFile <- 'C:/Users/u0214256/Documents/Keys/human_mortality_database.csv'  
hmd.loginData <- trimws(as.character(read.csv(hmd.loginFile, stringsAsFactors = FALSE)[1,]))
# Username and password for HMD. Replace these with the individual user's literal strings.
hmd.username <- hmd.loginData[1]
hmd.password <- hmd.loginData[2]

# Reads in the HMD codes. Note, these were adapted from the Hyndman's CRAN demography package. My require further updating if HMD has since altered the codes.
ccodes <- read.csv('./HMD Country Codes.csv', stringsAsFactors = FALSE)

# Rather than examine all countries, choose only select examples from around the world
countriesToExamine <- c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL')

# This line of code will examine all hmd countries: countriesToExamine <- ccodes$code

# A handy function to get factor "levels" into strings.
deLevel <- function(v){
  levels(v)[v]
}

# Get e0 from qx. This is a discrete lifetable function. It might be updated to use a different method as desired.
#  qx and agex are vectors for the respective lifetable parameters.
lxFromQx <- function(qx, agex){
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
  lx
}



# # Get the single-year period life tables from HMD for a given sex (male, female, or both). Can be one, two, or all three. Requires HMD username and password
# # Throws an error if there is a problem reading file (example, username or password invalid, bad country name, wrong input for sex specifications, changed file format, etc...)
# hmd.periodLifetab <- function(country, sex=c('m','f','b'), username, password, label = country) 
# {
#   if (!(sex %in% c('m','f','b'))){
#     stop('sex must be m, f, or b')
#   }
#   
#   taburl <- paste(sex, "ltper_1x1.txt", sep='')
#   path <- paste("http://www.mortality.org/hmd/", country, "/STATS/", 
#                taburl, sep = "")
#   userpwd <- paste(username, ":", password, sep = "")
#   txt <- RCurl::getURL(path, userpwd = userpwd)
#   con <- textConnection(txt)
#   lifetab <- try(utils::read.table(con, skip = 2, header = TRUE, 
#                               na.strings = "."), TRUE)
#   close(con)
#   if (class(lifetab) == "try-error") 
#     stop("Connection error at www.mortality.org. Please check username, password and country label.")
#   lifetab$country <- country
#   lifetab$sex <- sex
#   lifetab
# }

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
  clt2$lx <- lxFromQx(clt2$qx, clt2$age)
  # Done
  clt2
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

# print('Reading Period Data from HMD')
# # Read in the country-specific period life tables for the prespecified countries in the "countriesToExamine" variable.
# perlts <- pblapply(countriesToExamine, function(x0) {
#   print(x0)
#   lifetabs <- hmd.countryLifeTabs(x0, username=hmd.username, password=hmd.password)
#   SSRs <- hmd.countrySSRs(x0, username=hmd.username, password=hmd.password)
#   obj <- list(lifetabs, SSRs)
#   names(obj) <- c('lifetabs', 'SSRs')
#   obj
# })
# names(perlts) <- countriesToExamine


# This function does the first sex ratio analyses for a given set of year-specific life-table data, formatted like the HMD lifetables read in from the hmd.periodLifeTab function. (Note, this requires a country name field). It must be provided as the first item of a list in "datList". 
#  Either datList must have a second item that is a dataset of SSRs, with columns including 'Year' and 'SSR'; or, an override constant SSR must be provided (other than the default of NULL).
# The lowerBound parameter determines the first year for which the regression analyses begin.
# The first 
# Output includes: STILL WORKING ON THIS
srAnalyses <- function(datList, lowerBoundYear=1960, SSR=NA){
  if (class(datList) != 'list'){
    stop('datList must have class list')
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
  print(cName)
  
  # Get only the data since the lower bound year, and also get the "Year" variable into character format for plotting
  temp <- d1[d1$Year >= lowerBoundYear,]
  temp$year <- as.character(temp$Year)
  
  # Plots of select sex ratio curves since the lower bound year
  summaryCurve <- ggplot(temp[temp$Year %% 20 == 10 | temp$Year == 2000,], aes(x=age, y=lx.sr, colour=year)) + geom_line() + geom_hline(yintercept = 1, linetype=3) + labs(title=paste('Sex Ratio Curve\n',cName, 'Select Years'))
  
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
    # Among the ages where this is happening, the minimum age - 1 is the sex ratio crossover age
    return(data.frame(year=x$Year[1], ageCross=min(x$age[srcNotGreaterOne], na.rm=TRUE)-1))
  })))
  crossAgex$country <- country
  allCrossPoints <- crossAgex
  
  # Get only that subset since the lower bound year
  crossAgex <- allCrossPoints[allCrossPoints$year >= lowerBoundYear,]
  
  # Fit a linear model
  ageReg <- lm(ageCross ~ year, data=crossAgex)
  regSum <- summary(ageReg)
  linReg <- ageReg
  parms <- ageReg$coefficients

  # Place the fitted values in the dataset
  crossAgex$linFit <- parms[1] + crossAgex$year*parms[2]

  # A datset for storing key model info
  fitParms <- data.frame(country, linInt=parms[1], linBeta=parms[2], linR2=regSum$r.squared, stringsAsFactors = FALSE)
  
  ### This code tried to fit an exponential model. It did not work as anticipate. A better approach might be to fit a two-piece linear regression model. 
  # # Fit an exponential model
  # ageReg <- lm(log(ageCross) ~ year, data=crossAgex)
  # regSum <- summary(ageReg)
  # expReg <- ageReg
  # parms <- ageReg$coefficients
  # 
  # # Place the fitted values in the dataset
  # crossAgex$expFit <- exp(parms[1] + crossAgex$year*parms[2])
  # 
  # # key model info
  # fitParms$expInt <- parms[1]; fitParms$expBeta <- parms[2]; fitParms$expR2=regSum$r.squared;
  # rownames(fitParms) <- NULL
  
  # Plots
  
  basePlot <- ggplot(allCrossPoints, aes(x=year, y=ageCross)) + geom_point() + scale_y_continuous(limits=c(0,70)) + labs(title=paste('Sex Ratio Crossover by Year Period Life Tables\n',cName)) # all crossover points for the country
  linPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=linFit)) + labs(title=paste('Sex Ratio Crossover Linear Fit Period Life Tables\n',cName)) # Points (with linear fit) since the lowerBound year
  # expPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=expFit)) + labs(title=paste('Sex Ratio Crossover Exponential Fit Period Life Tables\n',cName))
  
  # Return objects of interest
  obj <- list(country, SSR, crossAgex, fitParms, linReg, basePlot, linPlot, allCurves, summaryCurve, origDat[order(origDat$Year, origDat$age),], allCrossPoints)
  names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'basePlot', 'linPlot', 'allCurves', 'summaryCurve', 'origDat', 'allCrossPoints')
  ### Again, comment out the version that fits the exponential model
  # obj <- list(country, SSR, crossAgex, fitParms, linReg, expReg, basePlot, linPlot, expPlot, allCurves, summaryCurve, origDat)
  # names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'expReg', 'basePlot', 'linPlot', 'expPlot', 'allCurves', 'summaryCurve', 'origDat')
  obj
}

# countryFits1 <- lapply(perlts, srAnalyses)
# 
# 
# # Plot them all on the same chart.
# fullCrossDat <- rBindThisList(lapply(countryFits1, function(x0) x0$allCrossPoints))
# 
# # pdf('./Plots/PeriodSRXSelect7.pdf')
# # print(ggplot(fullCrossDat[fullCrossDat$year >= 1970,], aes(x=year, y=ageCross, colour=country)) + geom_point() + labs(title='Sex Ratio Crossover for Select Countries\nfrom Period Life Tables') + ylab(label = 'Sex Ratio Crossover'))
# # No-title version
# print(ggplot(fullCrossDat[fullCrossDat$year >= 1970,], aes(x=year, y=ageCross, colour=country)) + geom_jitter() + xlab(label='Year') + ylab(label = 'Sex Ratio Crossover') )
# #dev.off()
# 
# # Plot just the SSRs for each country, since 1900
# allSSRs <- rBindThisList(lapply(perlts, function(x0) x0$SSRs))
# print(ggplot(allSSRs[allSSRs$Year >= 1900,], aes(x=Year, y=SSR, colour=country)) + geom_jitter() + geom_hline(yintercept=mean(allSSRs$SSR, na.rm=TRUE), linetype=3) + xlab(label='Year') + ylab(label = 'Secondary Sex Ratio') + labs(title='Secondary Sex Ratios by Country'))
# 
# # Each country individually
# lapply(split(allSSRs, allSSRs$country), function(x0){
#   ggplot(x0[x0$Year >= 1900,], aes(x=Year, y=SSR)) + geom_point() + geom_hline(yintercept=1.05, linetype=3) + xlab(label='Year') + ylab(label = 'Secondary Sex Ratio') + labs(title=paste(x0$country[1], 'Secondary Sex Ratios, with Mean')) + scale_y_continuous(limits=c(1.0,1.1)) # + geom_hline(yintercept=mean(x0$SSR, na.rm=TRUE), linetype=3)
# })
# 
# countryFits1$USA$allCurves$`2010` + ylab(label='Sex Ratio')

# Try it using the method of log(SSR) = area under the curve from age 0 to age x of the excess male mortality.
# Take the male minus the female mx. Get the cumulative sum. Find the minimum age x at which this is less than the natural log of the SSR.

srAnalysesMxMethod <- function(datList, lowerBoundYear=1960, SSR=NA){
  if (class(datList) != 'list'){
    stop('datList must have class list')
  }
  
  # Keep only the country, Year, Age, and mx variables
  colsToKeep <- c('country','Year','Age','mx')
  
  
  ltdat <- datList[[1]]
  ssrDat <- NULL
  if (length(datList) > 1){
    ssrDat <- datList[[2]]  
  }
  
  
  # Get the male and female data into a wideform (this includes separating datasets and then merging)
  f <- ltdat[ltdat$sex=='f', colsToKeep]
  colnames(f)[4] <- 'mx.f'
  m <- ltdat[ltdat$sex=='m', colsToKeep]
  colnames(m)[4] <- 'mx.m'
  d0 <- merge(f, m)
  
  # For each year in which a lifetable is available
  ltYears <- unique(ltdat$Year)
  
  if (!is.na(SSR)){
    ssrDat <- data.frame(Year=ltYears, SSR)
  }
  
  # Merge in the secondary sex ratio for each row
  d <- merge(d0, ssrDat[,c('Year','SSR')], all.x=TRUE)

  # Calculate the excess male mortality (DMR) for each age x
  d$DMR <- d$mx.m - d$mx.f

  # Get "Age" into numeric format
  d$age <- as.numeric(deLevel(d$Age))
  # Keep only up through age 100
  # Get the cumulative DMR and log SSR for each year
  temp <- rBindThisList(lapply(split(d, d$Year), function(x0){
    x <- x0[order(x0$age),]
    # The cumulative DMR
    x$cumDMR <- cumsum(x$DMR)
    x$lnSSR <- log(x$SSR)
    x
  }))
  
  d1 <- temp[!(is.na(temp$age)) & temp$age <= 100,]
  # Store these data as "origDat"
  origDat <- d1
  # Get the country and country name
  country <- d1$country[1]
  cName <- ccodes$country[ccodes$code==country]
  print(cName)
  
  # Get only the data since the lower bound year, and also get the "Year" variable into character format for plotting
  temp <- d1[d1$Year >= lowerBoundYear,]
  temp$year <- as.character(temp$Year)
  
  # Plots of select DRM's since the  lower bound year
  summaryDMR <- ggplot(temp[(temp$Year %% 20 == 10 | temp$Year == 2000) & temp$age <= 70,], aes(x=age, y=DMR, colour=year)) + geom_line() +  labs(title=paste('DMR\n',cName, 'Select Years'))
  summaryCumDMR <- ggplot(temp[(temp$Year %% 20 == 10 | temp$Year == 2000) & temp$age <= 70,], aes(x=age, y=cumDMR, colour=year)) + geom_line() + geom_hline(yintercept = log(1.05), linetype=3) + labs(title=paste('Cumulative DMR\n',cName, 'Select Years'))
  
  # Plots of all individual DMRs
  allCurves <- lapply(split(temp, temp$Year), function(x0){
    ggplot(x0, aes(x=age, y=DMR)) + geom_line() + labs(title=paste('DMR\n',cName, x0$Year[1])) 
  })
  
  # For each year, this finds the first (minimum) age at which the cumulative excess male mortality reaches the natural log of the SSR
  crossAgex <- as.data.frame(rbindlist(lapply(split(d1, d1$Year), function(x0){
    # Order the data points by age
    x <- x0[order(x0$age),]
    # Find every place the cumulative DMR is at least as large as the natural log of the SSR.
    largerLnSSR <- (x$cumDMR > x$lnSSR)
    # Crossover is the minimum age - 1
    return(data.frame(year=x$Year[1], ageCross=min(x$age[largerLnSSR], na.rm = TRUE)-1))
  })))

  crossAgex$country <- country
  allCrossPoints <- crossAgex
  
  # Get only that subset since the lower bound year
  crossAgex <- allCrossPoints[allCrossPoints$year >= lowerBoundYear,]
  
  # Fit a linear model
  ageReg <- lm(ageCross ~ year, data=crossAgex)
  regSum <- summary(ageReg)
  linReg <- ageReg
  parms <- ageReg$coefficients

  # Place the fitted values in the dataset
  crossAgex$linFit <- parms[1] + crossAgex$year*parms[2]

  # A datset for storing key model info
  fitParms <- data.frame(country, linInt=parms[1], linBeta=parms[2], linR2=regSum$r.squared, stringsAsFactors = FALSE)
  
  ### This code tried to fit an exponential model. It did not work as anticipate. A better approach might be to fit a two-piece linear regression model. 
  # # Fit an exponential model
  # ageReg <- lm(log(ageCross) ~ year, data=crossAgex)
  # regSum <- summary(ageReg)
  # expReg <- ageReg
  # parms <- ageReg$coefficients
  # 
  # # Place the fitted values in the dataset
  # crossAgex$expFit <- exp(parms[1] + crossAgex$year*parms[2])
  # 
  # # key model info
  # fitParms$expInt <- parms[1]; fitParms$expBeta <- parms[2]; fitParms$expR2=regSum$r.squared;
  # rownames(fitParms) <- NULL
  
  # Plots
  
  basePlot <- ggplot(allCrossPoints, aes(x=year, y=ageCross)) + geom_point() + scale_y_continuous(limits=c(0,70)) + labs(title=paste('Sex Ratio Crossover by Year Period Life Tables\n',cName)) # all crossover points for the country
  linPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=linFit)) + labs(title=paste('Sex Ratio Crossover Linear Fit Period Life Tables\n',cName)) # Points (with linear fit) since the lowerBound year
  # expPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=expFit)) + labs(title=paste('Sex Ratio Crossover Exponential Fit Period Life Tables\n',cName))
  
  # Return objects of interest
  obj <- list(country, SSR, crossAgex, fitParms, linReg, basePlot, linPlot, allCurves, summaryDMR, summaryCumDMR, origDat[order(origDat$Year, origDat$age),], allCrossPoints)
  names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'basePlot', 'linPlot', 'allCurves', 'summaryDMR','summaryCumDMR', 'origDat', 'allCrossPoints')
  ### Again, comment out the version that fits the exponential model
  # obj <- list(country, SSR, crossAgex, fitParms, linReg, expReg, basePlot, linPlot, expPlot, allCurves, summaryCurve, origDat)
  # names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'expReg', 'basePlot', 'linPlot', 'expPlot', 'allCurves', 'summaryCurve', 'origDat')
  obj
}

# countryFits2 <- lapply(perlts, srAnalysesMxMethod)
# 
# 
# # Compare the methods
# fullCrossDat2 <- rBindThisList(lapply(countryFits2, function(x0) x0$allCrossPoints))
# 
# colnames(fullCrossDat)[2]  <- 'lxCross'
# colnames(fullCrossDat2)[2] <- 'mxCross'
# 
# crossDatComp <- merge(fullCrossDat, fullCrossDat2)
# crossDatComp$diffCheck <- crossDatComp$lxCross-crossDatComp$mxCross
# table(crossDatComp$diffCheck)
# keepIt <- crossDatComp[crossDatComp$year >= 1850,]
# table(keepIt$diffCheck)
# test <- keepIt[keepIt$diffCheck == 2 & !is.na(keepIt$diffCheck),]
# # Examine cases where the difference is 2
# temp <- countryFits1$SWE$origDat
# temp2[temp2$Year==1934,]
# temp2 <- countryFits2$SWE$origDat
# temp2[temp2$Year==1902,]


# Check the 

# # pdf('./Plots/PeriodSRXSelect7.pdf')
# # print(ggplot(fullCrossDat[fullCrossDat$year >= 1970,], aes(x=year, y=ageCross, colour=country)) + geom_point() + labs(title='Sex Ratio Crossover for Select Countries\nfrom Period Life Tables') + ylab(label = 'Sex Ratio Crossover'))
# # No-title version
# print(ggplot(fullCrossDat[fullCrossDat$year >= 1970,], aes(x=year, y=ageCross, colour=country)) + geom_jitter() + xlab(label='Year') + ylab(label = 'Sex Ratio Crossover') )  
# #dev.off()
# 
# # Plot just the SSRs for each country, since 1900
# allSSRs <- rBindThisList(lapply(perlts, function(x0) x0$SSRs))
# print(ggplot(allSSRs[allSSRs$Year >= 1900,], aes(x=Year, y=SSR, colour=country)) + geom_jitter() + geom_hline(yintercept=mean(allSSRs$SSR, na.rm=TRUE), linetype=3) + xlab(label='Year') + ylab(label = 'Secondary Sex Ratio') + labs(title='Secondary Sex Ratios by Country'))  
# 
# # Each country individually
# lapply(split(allSSRs, allSSRs$country), function(x0){
#   ggplot(x0[x0$Year >= 1900,], aes(x=Year, y=SSR)) + geom_point() + geom_hline(yintercept=1.05, linetype=3) + xlab(label='Year') + ylab(label = 'Secondary Sex Ratio') + labs(title=paste(x0$country[1], 'Secondary Sex Ratios, with Mean')) + scale_y_continuous(limits=c(1.0,1.1)) # + geom_hline(yintercept=mean(x0$SSR, na.rm=TRUE), linetype=3)   
# })
# 
# countryFits1$USA$allCurves$`2010` + ylab(label='Sex Ratio')


print('Reading Cohort Data from HMD')
# Read in the country-specific period life tables for the prespecified countries in the "countriesToExamine" variable.
cohlts <- pblapply(countriesToExamine, function(x0) {
  print(x0)
  lifetabs <- hmd.countryLifeTabs(x0, username=hmd.username, password=hmd.password, ltTyp='cohort')
  SSRs <- hmd.countrySSRs(x0, username=hmd.username, password=hmd.password)
  obj <- list(lifetabs, SSRs)
  names(obj) <- c('lifetabs', 'SSRs')
  obj
})

names(cohlts) <- countriesToExamine
cohortFits <- lapply(cohlts, srAnalyses)

