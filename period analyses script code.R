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

# Get the single-year period life tables from HMD for a given sex (male, female, or both). Can be one, two, or all three. Requires HMD username and password
# Throws an error if there is a problem reading file (example, username or password invalid, bad country name, wrong input for sex specifications, changed file format, etc...)
hmd.periodLifetab <- function(country, sex=c('m','f','b'), username, password, label = country) 
{
  if (!(sex %in% c('m','f','b'))){
    stop('sex must be m, f, or b')
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
  lifetab
}

# Get the sigle-year period life tables for a given country. Calls the hmd.periodLifeTab function, and might throw associated errors.
hmd.countryPeriodLifeTabs <- function(country,  username, password){
  f <- hmd.periodLifetab(country, 'f', username, password)
  m <- hmd.periodLifetab(country, 'm', username, password)
  b <- hmd.periodLifetab(country, 'b', username, password)
  rbind(f,m,b)
}

print('Reading Period Data from HMD')
# Read in the country-specific period life tables for the prespecified countries in the "countriesToExamine" variable.
perlts <- pblapply(countriesToExamine, function(x0) {
  print(x0)
  hmd.countryPeriodLifeTabs(x0, username=hmd.username, password=hmd.password)
})
names(perlts) <- countriesToExamine


# This function does the first sex ratio analyses for a given set of year-specific life-table data, formatted like the HMD lifetables read in from the hmd.periodLifeTab function. (Note, this requires a country name field). The lowerBound parameter determines the first year for which the regression analyses begin.
# SSR is secondary sex ratio (ratio of males to females at birth. Default is 1.05.
# Output includes: STILL WORKING ON THIS
srAnalyses <- function(ltdat, lowerBoundYear=1960, SSR=1.05){
  # Keep only the country, Year, Age, and lx variables
  colsToKeep <- c('country','Year','Age','lx')
  # Get the male and female data into a wideform (this includes separating datasets and then merging)
  f <- ltdat[ltdat$sex=='f', colsToKeep]
  colnames(f)[4] <- 'lx.f'
  m <- ltdat[ltdat$sex=='m', colsToKeep]
  colnames(m)[4] <- 'lx.m'
  d <- merge(f, m)
  
  # Calculate the sex ratio for each row (i.e., by year and age)
  d$lx.sr <- SSR * d$lx.m / d$lx.f
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
    # Find every place the sex ratio is greater than 1, and store it as a 1.
    greaterOne <- (x$lx.sr > 1)*1
    # The sum of these 1's gives the number of age groups that are under age 1. Since they are single year of age data, and 
    # the first age groups is 0, just subtract 1 from the sum to get the necessary crossing age.
    ageCross <- sum(greaterOne)-1
    return(data.frame(year=x$Year[1], ageCross=ageCross))
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
  obj <- list(country, SSR, crossAgex, fitParms, linReg, basePlot, linPlot, allCurves, summaryCurve, origDat, allCrossPoints)
  names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'basePlot', 'linPlot', 'allCurves', 'summaryCurve', 'origDat', 'allCrossPoints')
  ### Again, comment out the version that fits the exponential model
  # obj <- list(country, SSR, crossAgex, fitParms, linReg, expReg, basePlot, linPlot, expPlot, allCurves, summaryCurve, origDat)
  # names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'expReg', 'basePlot', 'linPlot', 'expPlot', 'allCurves', 'summaryCurve', 'origDat')
  obj
}

countryFits1 <- lapply(perlts, srAnalyses)

# Write 
# writeToPDF <- function(thePlot, prefix){
#   country <- thePlot$data$country[1]
#   fileName <- paste('./Plots/', prefix, '_', country, '.pdf', sep='')
#   pdf(fileName)
#   print(thePlot)
#   dev.off()
# }

# Plot them all on the same chart.
fullCrossDat <- rBindThisList(lapply(countryFits1, function(x0) x0$allCrossPoints))

# pdf('./Plots/PeriodSRXSelect7.pdf')
# print(ggplot(fullCrossDat[fullCrossDat$year >= 1970,], aes(x=year, y=ageCross, colour=country)) + geom_point() + labs(title='Sex Ratio Crossover for Select Countries\nfrom Period Life Tables') + ylab(label = 'Sex Ratio Crossover'))
# No-title version
print(ggplot(fullCrossDat[fullCrossDat$year >= 1970,], aes(x=year, y=ageCross, colour=country)) + geom_jitter() + xlab(label='Year') + ylab(label = 'Sex Ratio Crossover') )  
#dev.off()

countryFits1$USA$allCurves$`2010` + ylab(label='Sex Ratio')



lapply(countryFits1, function(x0) writeToPDF(x0$basePlot, 'basecrosses'))
# lapply(countryFits1, function(x0) x0$linPlot)