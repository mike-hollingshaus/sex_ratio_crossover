# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# mike.hollingshaus@utah.edu
# Purpose: This is the main user interface script for replicating the work.
# Instructions. 
#   User sets: 
#     1. The path for the working directory where the five script files and the ccodes dataset is located.
#     2. The path for a personal folder containing the output objects. Can be the same as the first folder, if desired. However, it should not be linked to a public repository.
#     2. A personal folder for storing the sex ratio analysis outputs.
#     4. HMD username and password.
#     5. A vector of strings representing the countries to be examined (using the HMD abbreviations). Default is c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL').
#     6. The first secular year analysis is desired (default is 1850)
#     7. The user may optionally set full country names, which are used in the "Figures and Tables" script.
#  Once these are set, run the entire file. The "HMD Interface ..." script runs analyses, and stores objects on the local disk in the "personal folder". 
#  The "Figures and Tables" script generates figures and tables that appear in the paper, along with other data included in the analysis. That second script might need to be adapted for a specific analysis.

# Empty out memory
rm(list=ls())

# Set the working directory to the local repository
setwd('')
personalFolder <- ''

# Username and password for HMD. Replace these with the individual user's literal strings.
hmd.username <- ''
hmd.password <- ''

# Rather than examine all countries, choose only select examples from around the world
countriesToExamine <- c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL')
# This variable is not technically required for analysis, but it formats the output found in the paper. If not included, the "Figures and Tables Script" won't work.
fullCountryNames <- c('United States', 'Sweden', 'Australia', 'Japan', 'Russia', 'Italy', 'Chile')

# An optional parameter to limit analyses to a secular period after a given year
earliestAnalysisYear <- 1850

# Run the analyses
print('Reading HMD Data, and generating sex ratio objects')
source('HMD Interface and Sex Ratio Analyses Scripts.R')
print('Creating plots and tables')
source('Figures and Tables.R')

