# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# mike.hollingshaus@utah.edu
# Last update: 2018-02-28.
# As of this date, the package is also publicly available at: https://github.com/mike-hollingshaus/sex_ratio_crossover

# This README file is associated with Sex Ratio Curves and Crosses Analyses for seven developed countries.



# Files #
This package includes four .R script files, and one .csv file. 
1. "sex ratio crossover _main.R" - This is the main user interface. It will source the code in the other files as necessary.
2. "HMD Interface and Sex Ratio Analyses Scripts.R" - This contains the main functions for reading Human Mortality Database (HMD) data and constructing the sex ratio crossover objects. It is called from the "_main" file. These are the most substantially important scripts for the analysis.
3. "Figures and Tables.R" - This generates the figures and tables in the paper. It is called from the "_main" file.
4. "color schemes.R" - This has code that generates colors used in the plots.
5. "HMD Country Codes.csv" - This is a .csv file that includes available country codes for the HMD.


# Instructions #
1. Place all five files in the same folder.
2. Adjust inputs in the "sex ratio crossover _main.R" script. User should set:
   a. The working directory for the five files.
   b. A personal directory or folder for outputs.
   c. HMD username and password.
   d. A string vector of the countries to be examined, using the HMD abbreviations. It is currently set to the paper's default 7 countries, but can be changed.	
	(Note that the abbreviations are stored in the .csv file for reference)
   e. Full country names. They should map directly to the abbreviated HMD names, and are used in the "Figures and Tables" script.
   f. The earliest year for analysis. Default is 1850.
3. Run the script. The formatted sex ratio objects and plots should appear in the personal folder. Details on the objects themselves are found in the associated scripts.






