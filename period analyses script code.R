
library(data.table)
library(ggplot2)
library(demography)
rm(list=ls())
cat('\014')

deLevel <- function(v){
  levels(v)[v]
}



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
countriesToExamine <- c('USA', 'SWE', 'AUS', 'CAN', 'GBR_NP')

perlts <- lapply(countriesToExamine, hmd.countryPeriodLifeTabs, username=hmd.username, password=hmd.password)
names(perlts) <- countriesToExamine

srAnalyses <- function(ltdat){
  colsToKeep <- c('country','Year','Age','lx')
  f <- ltdat[ltdat$sex=='f', colsToKeep]
  colnames(f)[4] <- 'lx.f'
  m <- ltdat[ltdat$sex=='m', colsToKeep]
  colnames(m)[4] <- 'lx.m'
  d <- merge(f, m)
  d$lx.sr <- 1.05 * d$lx.m / d$lx.f
  d$age <- as.numeric(deLevel(d$Age))
  # Keep only up through age 100
  d1 <- d[!(is.na(d$age)) & d$age <= 100,]
  
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
  crossAgex$country <- d1$country[1]
  
  ageReg <- lm(ageCross ~ year, data=crossAgex)
  summary(ageReg)
  
  # Make a plot
  
  print('d')
}

lapply(perlts, srAnalyses)