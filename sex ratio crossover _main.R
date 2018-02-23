# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: This is the main user interface script for replicating the work.
# Instructions. 
#   User sets: 
#     1. the path for the working directory where the four script files and the ccodes dataset is located.
#     2. the path for a personal folder containing the output objects. Can be the same as the first folder, if desired. However, it should not be linked to a public repository.
#     2. A personal folder for storing the sex ratio analysis outputs
#     4. HMD username and password
#     5. A vector of strings representing the countries to be examined (using the HMD abbreviations). Default is c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL')
#     6. The first secular year analysis is desired (default is 1850)
#  Once these are set, run the entire file. The "HMD Interface ..." script runs analyses, and stores objects on the local disk in the "personal folder". The "Figures and Tables" script generaetes figures and tables, along with other data included in the analysis.

# Empty out memory
rm(list=ls())

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


