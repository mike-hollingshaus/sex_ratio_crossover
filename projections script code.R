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

# Read in the Census Bureau Projections
