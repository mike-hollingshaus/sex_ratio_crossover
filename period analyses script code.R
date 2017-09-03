
library(data.table)
library(ggplot2)
library(demography)
library(pbapply)
rm(list=ls())
cat('\014')

deLevel <- function(v){
  levels(v)[v]
}

ccodes <- read.csv('./HMD Country Codes.csv', stringsAsFactors = FALSE)


# Get the 
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

hmd.countryPeriodLifeTabs <- function(country,  username, password){
  f <- hmd.periodLifetab(country, 'f', username, password)
  m <- hmd.periodLifetab(country, 'm', username, password)
  b <- hmd.periodLifetab(country, 'b', username, password)
  rbind(f,m,b)
}

hmd.loginFile <- 'C:/Users/u0214256/Documents/Keys/human_mortality_database.csv'  
hmd.loginData <- trimws(as.character(read.csv(hmd.loginFile, stringsAsFactors = FALSE)[1,]))
hmd.username <- hmd.loginData[1]
hmd.password <- hmd.loginData[2]
# countriesToExamine <- c('USA', 'SWE', 'AUS', 'CAN', 'GBR_NP')
countriesToExamine <- ccodes$code
perlts <- pblapply(countriesToExamine, function(x0) {
  print(x0)
  hmd.countryPeriodLifeTabs(x0, username=hmd.username, password=hmd.password)
})
names(perlts) <- countriesToExamine

srAnalyses <- function(ltdat){
  colsToKeep <- c('country','Year','Age','lx')
  f <- ltdat[ltdat$sex=='f', colsToKeep]
  colnames(f)[4] <- 'lx.f'
  m <- ltdat[ltdat$sex=='m', colsToKeep]
  colnames(m)[4] <- 'lx.m'
  d <- merge(f, m)
  SSR <- 1.05
  d$lx.sr <- SSR * d$lx.m / d$lx.f
  d$age <- as.numeric(deLevel(d$Age))
  # Keep only up through age 100
  d1 <- d[!(is.na(d$age)) & d$age <= 100,]
  country <- d1$country[1]
  print(paste('\n--',country,'--\n'))
  
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
  
  crossAgex <- allCrossPoints[allCrossPoints$year >= 1950,]
  
  # Fit a linear model
  ageReg <- lm(ageCross ~ year, data=crossAgex)
  regSum <- summary(ageReg)
  linReg <- ageReg
  parms <- ageReg$coefficients
  
  # Place the fitted values in the dataset
  crossAgex$linFit <- parms[1] + crossAgex$year*parms[2]

  # A datset for storing key model info
  fitParms <- data.frame(country, linInt=parms[1], linBeta=parms[2], linR2=regSum$r.squared, stringsAsFactors = FALSE)
  
  # Fit an exponential model
  ageReg <- lm(log(ageCross) ~ year, data=crossAgex)
  regSum <- summary(ageReg)
  expReg <- ageReg
  parms <- ageReg$coefficients
  
  # Place the fitted values in the dataset
  crossAgex$expFit <- exp(parms[1] + crossAgex$year*parms[2])
  
  # key model info
  fitParms$expInt <- parms[1]; fitParms$expBeta <- parms[2]; fitParms$expR2=regSum$r.squared;
  rownames(fitParms) <- NULL
  cName <- ccodes$country[ccodes$code==country]
  
  # Make a plot
  basePlot <- ggplot(allCrossPoints, aes(x=year, y=ageCross)) + geom_point() + labs(title=paste('Sex Ratio Crossover by Year Period Life Tables\n',cName))
  linPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=linFit)) + labs(title=paste('Sex Ratio Crossover Linear Fit Period Life Tables\n',cName))
  expPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=expFit)) + labs(title=paste('Sex Ratio Crossover Exponential Fit Period Life Tables\n',cName))
  
  # Return objects of interest
  obj <- list(country, SSR, crossAgex, fitParms, linReg, expReg, basePlot, linPlot, expPlot)
  names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'expReg', 'basePlot', 'linPlot', 'expPlot')
  obj
}

countryFits1 <- lapply(perlts, srAnalyses)
lapply(countryFits1, function(x0) x0$basePlot)
lapply(countryFits1, function(x0) x0$linPlot)
lapply(countryFits1, function(x0) x0$expPlot)
