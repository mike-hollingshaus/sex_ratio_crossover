# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: Sex ratio crossovers from projections data

library(data.table)
library(ggplot2)
library(demography)
library(pbapply)

# Empty out memory
rm(list=ls())
# A prefrence function to clear the console
cls <- function() cat('\014')
cls()

selectYearsToPlot <- c(2014, seq(from=2020,to=2060,by=10))

# A convenience function to rbind a list using the data.table package, but cast it back into data.frame format
rBindThisList <- function(theList){
  as.data.frame(rbindlist(theList))
}
# Set the working directory to the local repository
setwd('D:/Current Projects/Mortality Sex Ratio Changes/sex_ratio_crossover')

# A handy function to get factor "levels" into strings.
deLevel <- function(v){
  levels(v)[v]
}

# Read in the Census Bureau US Projections
cb0 <- read.csv('./Projections Data/Census Bureau US/CensBur 2014 US Proj AgeSexRaceOrig 20170912.csv', stringsAsFactors = FALSE)

# We need all races and origins (race=0, sex=0), and male and female (sex=1, 2). Also, the race and origin columns will be unnecessary, as is the total population column. So remove them.
cb <- cb0[cb0$origin==0 & cb0$race==0 & cb0$sex %in% 1:2,-c(1,2,5)]
# Transform the ages to long form variables
cb.long <- melt(cb, c('sex','year'), variable.name='ageVar')
# Extract a numeric age from the age variable names
cb.long$ageVar <- deLevel(cb.long$ageVar)
cb.long$age <- as.integer(substr(cb.long$ageVar, 5, nchar(cb.long$ageVar)))

# Separate the males and females, and merge into one dataset.
female <- cb.long[cb.long$sex==2,]
male <- cb.long[cb.long$sex==1,]
colnames(female)[4] <- 'female'
colnames(male)[4]   <- 'male'
cb.srd <- merge(male[,c(2,4,5)], female[,c(2,4,5)])
cb.srd$SR <- cb.srd$male/cb.srd$female

# Plot the sex ratio curves over time.
cb.srd$Year <- as.character(cb.srd$year)

# Look at all the SRX's by year.
# For a given year, this function finds the first (minimum) age at which the sex ratio dips below 1.
# d is a data.frame with three columns. The first is the year (need not be numeric) the second is age and the third the sex ratio at that age
crossAgexF <- function(d) {
  # First, get the data column names set
  colnames(d) <- c('year','age','SR')
  # Now order by age
  d1 <- d[order(d$age),]  
  # Find every place the sex ratio is greater than 1, and store it as a 1.
  greaterOne <- (d1$SR > 1)*1
  # The sum of these 1's gives the number of age groups that are under age 1. Since they are single year of age data, and 
  #   the first age groups is 0, just subtract 1 from the sum to get the necessary crossing age.
  ageCross <- sum(greaterOne)-1
  return(data.frame(year=d$year[1], SRX=ageCross))
}
  
cbForm <- cb.srd[,c('year','age','SR')]
allCBCross <- rBindThisList(lapply(split(cbForm, cbForm$year), crossAgexF))

# Plot the Sex Ratio Curves and Crosses
ggplot(cb.srd[cb.srd$year %in% selectYearsToPlot,], aes(x=age, y=SR, colour=Year)) + geom_line() + geom_hline(yintercept=1, linetype=3) + labs(title='Projected US Sex Rato Curves\nDerived from US Census Bureau 2014 Projections')
ggplot(allCBCross, aes(x=year, y=SRX)) + geom_point() + scale_y_continuous(limits=c(0,70)) + labs(title='Projected U.S. Sex Ratio Crossover Ages\nDerived from Census Bureau 2014 Projections')

# Print the actual data
print(allCBCross)
# Goes from age 36 in 2014 to age 62 in 2060
