
library(data.table)
library(ggplot2)
library(demography)
library(pbapply)

# Empty out memory
rm(list=ls())
# A prefrence function to clear the console
cls <- function() cat('\014')
cls()

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

