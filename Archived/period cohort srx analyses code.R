# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: Read data from Human Mortality Database. construct period and cohort sex ratio curve and crossover objects. Write them to disk.
# Required packages: data.table, pbapply
#  The functions that start with 'hmd' were adapted from code by Hyndman et al. in the package 'demography'

# Instructions

# Empty out memory
rm(list=ls())

# Load packages
library(data.table) # data manipulation functions
library(pbapply) # progress bar for the 'lapply()' function

# Set the working directory to the local repository
setwd('D:/Current Projects/Mortality Sex Ratio Changes/sex_ratio_crossover')
personalFolder <- 'D:/Current Projects/Mortality Sex Ratio Changes/SRX Objects with Raw HMD Data/'

# A file on the local harddrive for reading in the author's username/password for HMD. In practice, comment these out, and hardcode the user's username and password with literal strings. 
hmd.loginFile <- 'C:/Users/u0214256/Documents/Keys/human_mortality_database.csv'  
hmd.loginData <- trimws(as.character(read.csv(hmd.loginFile, stringsAsFactors = FALSE)[1,]))

# Username and password for HMD. Replace these with the individual user's literal strings.
hmd.username <- hmd.loginData[1]
hmd.password <- hmd.loginData[2]

# Rather than examine all countries, choose only select examples from around the world
countriesToExamine <- c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL')
# An optional parameter to limit analyses to a secular period after a given year
earliestAnalysisYear <- 1850

# Run the analyses
source('HMD Interface and Sex Ratio Analyses Scripts.R')


