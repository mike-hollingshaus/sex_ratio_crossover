# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: Generates Figures and Tables, and other data for document. Requires packages data.table and ggplot2
#             This can be run right after the HMD Interface ... script. However, it can also load previously-saved sex ratio analyses data objects
#             Tables are simply printed, but plots are written to disk, and formmated to Demographic Research specifications.

library(data.table)
library(ggplot2)

# This line of code will load the objects saved in HMD Interface and Sex Ratio Analyses, if required. It is useful to load objects generated from a specific data pull, in case HMD updates the data.
# load('D:/Current Projects/Mortality Sex Ratio Changes/SRX Objects with Raw HMD Data/PeriodCohortSRXObjects_2018-02-02')

# Plotting preferences to meet journal Demographic Research guidelines (with some personal preferences)
plotWidth <- 12.46
plotHeight <- (3/5)*plotWidth
shapeSize <- .7
fontSize <- 7
fontType <- 'sans'
plotRes <- 300
plotUnits <- 'cm'
plotTheme <- function() theme_bw(base_size=fontSize) + theme(axis.title=element_text(size=fontSize))

# This helper function will take a plot, and save it in .tiff format as the specified filename. Plot specifications (and path) are as defined above.
writePlotAsTiff <- function(thePlot, fileName){
  tiff(paste(personalFolder, fileName, '.tiff', sep=''), width=plotWidth, height=plotHeight, units=plotUnits, res=plotRes, family=fontType) 
  print(thePlot + plotTheme())
  dev.off()
}

# Rather than examine all countries, choose only select examples from around the world
# countriesToExamine <- c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL')
names(fullCountryNames) <- countriesToExamine

# Load in the color schemes for plotting. They are stored in a separate script.
source('color schemes.R')
per_coh_colours <- c(sr_cols$primary$`Masters Black`, sr_cols$primary$Red)
# A Map for country colors
country_color_map <- 
  c(
    sr_cols$tertiary$`Amber Yellow`,
    sr_cols$tertiary$`Amazon Green`,
    sr_cols$tertiary$`Azure Blue`,
    sr_cols$primary$`Masters Black`,
    sr_cols$secondary$`Wood`,
    sr_cols$primary$`Granite Grey`,
    sr_cols$primary$`Red`
  )
names(country_color_map) <- fullCountryNames[order(fullCountryNames)]
# Shapes for country points
countryShapes <- c(1:4,6:8)
names(countryShapes) <- countriesToExamine

# Figure 1 - Period Sex Ratio Curve for U.S. 2010 period lifetable. Note that, when saved as a .tiff, the scales change.
temp <- perFits$USA$origDat # The US original data
plotDat <- temp[temp$Year==2010,] # The US 2010 data
srcPlot <- ggplot(plotDat, aes(x=age, y=lx.sr)) + geom_line() + xlab(label='Age') + ylab(label='Sex ratio') + geom_hline(yintercept=1, linetype=3) + geom_vline(xintercept=56, linetype=3) + scale_y_continuous(limits=c(.3,1.1), breaks=seq(.4,1.2,.2)) + annotate('text', x=54.5, y=.6, label='Sex ratio crossover: 56', angle=90, size=1.2) + annotate('text', x=10, y=1.07, label='Secondary sex ratio: 1.05', size=1.2) # The plot
writePlotAsTiff(srcPlot, 'USA2010SRC') # Write the plot to personal folder, if desired.

# Figure 2, showing 2010 female and male m(x)
usa2010 <- perdat$USA$lifetabs[perdat$USA$lifetabs$Year==2010,c('sex','Age','mx')] # US 2010 data
u10m <- usa2010[usa2010$sex=='m',] # Male
u10f <- usa2010[usa2010$sex=='f',] # Female
colnames(u10m)[3] <- 'm.mx' # New column name
colnames(u10f)[3] <- 'f.mx' # New column name
usa2010.2 <- merge(u10m[,c('Age','m.mx')], u10f[,c('Age','f.mx')]) # Merged file
# Get numeric age, and order data
usa2010.2$age <- as.numeric(deLevel(usa2010.2$Age)) # Numeric age
usa2010.2 <- usa2010.2[order(usa2010.2$age),] # Order the data
plotDat <- melt(usa2010.2[,c('age','m.mx','f.mx')], 'age') # The plot data in long form
plotDat2 <- plotDat[plotDat$age <= 60 & !is.na(plotDat$age),] # only ages 60 and under
# The next two lines make sex variable a character string so the legend is properly formatted
plotDat2$variable <- deLevel(plotDat2$variable) 
plotDat2$Sex <- ifelse(plotDat2$variable=='m.mx', 'Male', 'Female')
mxBySexPlot <- ggplot(plotDat2, aes(x=age, y=value, linetype=Sex, colour=Sex)) + geom_line() + ylab(label='Mortality rate m(x)') + xlab(label='Age') + scale_linetype_manual(values=1:2) + scale_color_manual(values=per_coh_colours) # The plot
writePlotAsTiff(mxBySexPlot, 'fMx by sex USA 2010')  # Save plot to disk

# Figure 3 - US 2010 male-female DMR and CDMR
usa2010.2$dmr  <- usa2010.2$m.mx-usa2010.2$f.mx # excess male mortality
usa2010.2$cdmr <- cumsum(usa2010.2$dmr) # cumulative excess male mortality
plotDat <- melt(usa2010.2[usa2010.2$age <= 60,c('age','dmr','cdmr')], 'age') # Get into long form
# The next three lines prepare the variables as character strings to format the legend.
plotDat$variable <- deLevel(plotDat$variable) 
plotDat$Variable <- ifelse(plotDat$variable=='dmr', 'DMR', 'Cumulative DMR')
plotDat$Metric <- plotDat$Variable
# These two lines get the log of the secondary sex ratio in 2010
us10SSR <- perdat$USA$SSRs$SSR[perdat$USA$SSRs$Year==2010] 
log_us10SSR <- log(us10SSR)
# The actual plots
cmdrPlot <- ggplot(plotDat, aes(x=age, y=value, linetype=Metric, colour=Metric)) + geom_line() + ylab(label='Difference in m(x)') + xlab(label='Age') + scale_linetype_manual(values=1:2) + geom_hline(yintercept=log_us10SSR, linetype=3) + geom_vline(xintercept=55, linetype=3) + annotate('text', x=53.5, y=.02, label='Sex ratio crossover: 55', angle=90, size=2) + annotate('text', x=20, y=.051, label='Natural log of secondary sex ratio: 0.047', size=2) + scale_color_manual(values=per_coh_colours) # The plot
writePlotAsTiff(cmdrPlot, 'cmdr Plot') # Write the plot to disk

# Figures 4 and 5.
# First, get the srx data into one long form dataset for each country and all years. For both period and cohort fits.
perPlotDat <- rBindThisList(lapply(perFits, function(x0) x0$srxDat)) # perFullCrossDat[perFullCrossDat$year >= earliestAnalysisYear,]
cohPlotDat <- rBindThisList(lapply(cohFits, function(x0) x0$srxDat)) # cohFullCrossDat[cohFullCrossDat$year >= earliestAnalysisYear,]

# This function will generate the period or cohort plots
plotCountries <- function(plotDat, type='Period'){
  plotDat$country <- fullCountryNames[plotDat$country] # Get the full country name
  # These three lines will format the country names, in alphabetical order, for display in the legend
  legNames0 <- unique(plotDat$country)
  legNames <- legNames0[order(legNames0)]
  plotDat$Country <- factor(plotDat$country, levels=legNames)
  
  # Assignt the country shapes
  cShapes2 <- countryShapes
  names(cShapes2) <- fullCountryNames[names(cShapes2)]
  # The next line generates and returns the plot
  ggplot(plotDat, aes(x=year, y=srx, shape=Country, colour=Country)) + geom_jitter(size=shapeSize) + xlab(label='Year') + ylab(label = 'Sex ratio crossover') + scale_shape_manual(values=cShapes2[legNames]) + scale_color_manual(values=country_color_map[legNames])
}
# Plot period SRX - Figure 4
perPlots <- plotCountries(perPlotDat)
# Plot cohort SRX - Figure 5
cohPlots <- plotCountries(cohPlotDat, type='Cohort')
# Write the two plots to disk
writePlotAsTiff(perPlots, 'Period SRX')
writePlotAsTiff(cohPlots, 'Cohort SRX')

# Period vs. Cohort - Figure 6
# First, get the period and cohort datasets into one larger dataset
perPlotDat$Type <- 'Period' # Delineate type period
cohPlotDat$Type <- 'Cohort' # Delineate type cohort
bigDat <- rbind(perPlotDat, cohPlotDat) # Append them together
# Get the full country names
bigDat$Country <- fullCountryNames[bigDat$country] 
# Select the countries to plot. Here, there are four. This must be updated if other countries are included.
countriesToPlot <- c('Australia', 'Italy', 'United States', 'Sweden')
# Generate the actual plot, and write to disk
pvcPlot <- ggplot(bigDat[bigDat$Country %in% countriesToPlot,], aes(x=year, y=srx, shape=Type, colour=Type)) + geom_jitter(size=shapeSize) + xlab(label='Year') + ylab(label = 'Sex ratio crossover') + facet_wrap(~Country, scales='fixed') + scale_shape_manual(values=c(1,4)) + scale_color_manual(values=per_coh_colours)
writePlotAsTiff(pvcPlot, 'Cohort vs Period')

# Descriptive Statistics

# Table 1. Descriptive table for period sex ratio crossovers
allPerSRX <- perPlotDat # The dataset
# Get the range of years for each country into one column, with a hyphen separating first from last year
yearRangeList <- lapply(split(allPerSRX, allPerSRX$country), function(x0) range(x0$year)) 
cname <- names(yearRangeList)
psrxDat <- data.frame(country=cname, matrix(unlist(yearRangeList), nrow=7, byrow=T))
colnames(psrxDat)[2:3] <- c('minyear','maxyear')
psrxDat$yearRange <- paste(psrxDat$minyear, psrxDat$maxyear, sep='-')
# The min, max, mean, and standard deviation of SRX for each country
psrxDat$minSRX <-  unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) min(x0$srx, na.rm=TRUE)))
psrxDat$maxSRX <-  unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) max(x0$srx, na.rm=TRUE)))
psrxDat$meanSRX <- unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) mean(x0$srx, na.rm=TRUE)))
psrxDat$stdev <- unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) sd(x0$srx, na.rm=TRUE)))
# Some SRX are missing, meaning that they never cross
# This function will find them, if they exist
missTabDat <- rBindThisList(lapply(split(allPerSRX, allPerSRX$country), function(x0) {
  missYears0 <- x0$year[is.na(x0$srx)]
  if (length(missYears0) > 0){
    stop('There was an NA age cross found!')
  }
  yearDiffs <- diff(x0$year)
  missYearsStr <- 'None'
  atLeastOneMissingYear <- any(yearDiffs > 1)
  if (atLeastOneMissingYear){
    # Get the indices of the year-diffs greater than 1
    diffInd <- which(yearDiffs > 1)
    # Find the years that are immediately previous to missing years
    prevYears <- x0$year[diffInd]
    # Also, immediately afterwards
    postYears <- x0$year[diffInd+1]
    missYears <- character()
    for (i in 1:length(diffInd)){
      missYears <- c(missYears, paste(prevYears[i]+1, postYears[i]-1, sep='-'))
    }
    # The missing years are in between
    missYearsStr <- paste(missYears, collapse=', ')
  }
  data.frame(country=x0$country[1], noCrossYears=missYearsStr)
}))
# Include them in the dataset
psrxDat <- merge(psrxDat, missTabDat)
psrxDat <- psrxDat[,c('country','yearRange','minSRX','maxSRX','meanSRX','stdev','noCrossYears')]
# Extend to the full sample
allPSRXDat <- psrxDat[1,]; allPSRXDat$country <- 'All'
allPSRXDat$yearRange <- paste(min(allPerSRX$year, na.rm=TRUE), max(allPerSRX$year, na.rm=TRUE), sep='-')
allPSRXDat$minSRX <- min(allPerSRX$srx, na.rm=TRUE); allPSRXDat$maxSRX <- max(allPerSRX$srx, na.rm=TRUE); allPSRXDat$meanSRX <- mean(allPerSRX$srx, na.rm=TRUE)
allPSRXDat$stdev <- sd(allPerSRX$srx, na.rm=TRUE)
allPSRXDat$noCrossYears <- NA
# Append them together, and print
print(psrxTab <- rbind(allPSRXDat, psrxDat))

# Descriptive table for cohort sex ratio crossovers
allCohSRX <- cohPlotDat
# Get the range of years
yearRangeList <- lapply(split(allCohSRX, allCohSRX$country), function(x0) range(x0$year))
cname <- names(yearRangeList)
csrxDat <- data.frame(country=cname, matrix(unlist(yearRangeList), nrow=length(cname), byrow=T))
colnames(csrxDat)[2:3] <- c('minyear','maxyear')
csrxDat$yearRange <- paste(csrxDat$minyear, csrxDat$maxyear, sep='-')
# SRX descriptives
csrxDat$minSRX <-  unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) min(x0$srx, na.rm=TRUE)))
csrxDat$maxSRX <-  unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) max(x0$srx, na.rm=TRUE)))
csrxDat$meanSRX <- unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) mean(x0$srx, na.rm=TRUE)))
csrxDat$stdev <- unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) sd(x0$srx, na.rm=TRUE)))
# Order the countries
csrxDat$country <- deLevel(csrxDat$country)
csrxDat <- csrxDat[order(csrxDat$country),]
# Preliminary examinations showed no missing data years in the middle of the range, as occurred with period. Move on to the full table
csrxDat <- csrxDat[,c('country','yearRange','minSRX','maxSRX','meanSRX','stdev')]
# Extend to the full sample
allcsrxDat <- csrxDat[1,]; allcsrxDat$country <- 'All'
allcsrxDat$yearRange <- paste(min(allCohSRX$year), max(allCohSRX$year), sep='-')
allcsrxDat$minSRX <- min(allCohSRX$srx, na.rm=TRUE); allcsrxDat$maxSRX <- max(allCohSRX$srx, na.rm=TRUE); allcsrxDat$meanSRX <- mean(allCohSRX$srx, na.rm=TRUE)
allcsrxDat$stdev <- sd(allCohSRX$srx, na.rm=TRUE)
# Append them together, and print the table
print(csrxTab <- rbind(allcsrxDat, csrxDat))

# This code generated descriptive statistics for secondary sex ratios, though the full table was not included in the paper.
# Basic descriptive statistics for SSRs
allSSRs <- rBindThisList(lapply(perdat, function(x0) x0$SSRs))
allSSRs <- allSSRs[allSSRs$Year >= 1850,]
yearRangeList <- lapply(split(allSSRs, allSSRs$country), function(x0) range(x0$Year))
cname <- names(yearRangeList)
# Store in a table
ssrDat <- data.frame(country=cname, matrix(unlist(yearRangeList), nrow=7, byrow=T))
colnames(ssrDat)[2:3] <- c('minyear','maxyear')
ssrDat$yearRange <- paste(ssrDat$minyear, ssrDat$maxyear, sep='-')
ssrDat$minSSR <-  unlist(lapply(split(allSSRs, allSSRs$country), function(x0) min(x0$SSR, na.rm = TRUE)))
ssrDat$maxSSR <-  unlist(lapply(split(allSSRs, allSSRs$country), function(x0) max(x0$SSR, na.rm = TRUE)))
ssrDat$meanSSR <- unlist(lapply(split(allSSRs, allSSRs$country), function(x0) mean(x0$SSR, na.rm = TRUE)))
ssrDat$stdev_100 <- unlist(lapply(split(allSSRs, allSSRs$country), function(x0) sd(x0$SSR, na.rm = TRUE)*100))
ssrDat <- ssrDat[,c('country','yearRange','minSSR','maxSSR','meanSSR','stdev_100')]
# Extend to the full sample
allSSRDat <- ssrDat[1,]; allSSRDat$country <- 'All'
allSSRDat$yearRange <- paste(min(allSSRs$Year), max(allSSRs$Year), sep='-')
allSSRDat$minSSR <- min(allSSRs$SSR, na.rm = TRUE); allSSRDat$maxSSR <- max(allSSRs$SSR, na.rm = TRUE); allSSRDat$meanSSR <- mean(allSSRs$SSR, na.rm = TRUE)
allSSRDat$stdev_100 <- sd(allSSRs$SSR, na.rm = TRUE)*100
# Append them together
print(ssrTab <- rbind(allSSRDat, ssrDat))
allSSRs <- rBindThisList(lapply(perdat, function(x0) x0$SSRs))
mean(allSSRs$SSR, na.rm = T)
sd(allSSRs$SSR, na.rm = T)

