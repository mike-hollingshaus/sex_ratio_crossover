# Author: Mike Hollingshaus
# Demographer
# Kem C. Gardner Policy Institute
# University of Utah
# Purpose: Generates Figures and Tables, and other data for document.

library(data.table)
library(ggplot2)
library(demography)

# Empty out memory
rm(list=ls())

# A prefrence function to clear the console
cls <- function() cat('\014')
cls()

# Load Data
load('D:/Current Projects/Mortality Sex Ratio Changes/SRX Objects with Raw HMD Data/PeriodCohortSRXObjects_2018-02-02')

# A convenience function to rbind a list using the data.table package, but cast it back into data.frame format
rBindThisList <- function(theList){
  as.data.frame(rbindlist(theList))
}
# Set the working directory to the local repository
setwd('D:/Current Projects/Mortality Sex Ratio Changes/sex_ratio_crossover')
plotDir <- 'D:/Current Projects/Mortality Sex Ratio Changes/sex_ratio_crossover/Formatted Plots/'
plotWidth <- 12.46
plotHeight <- (3/5)*plotWidth # 14.5 
shapeSize <- .7
fontSize <- 7
fontType <- 'sans'
plotRes <- 300
plotUnits <- 'cm'
plotTheme <- function() theme_bw(base_size=fontSize) + theme(axis.title=element_text(size=fontSize))
startYear <- 1850

writePlotAsTiff <- function(thePlot, fileName){
  tiff(paste(plotDir, fileName, '.tiff', sep=''), width=plotWidth, height=plotHeight, units=plotUnits, res=plotRes, family=fontType) 
  print(thePlot + plotTheme())
  dev.off()
}


# Load in the color schemes
source('color schemes.R')

# Reads in the HMD codes. Note, these were adapted from the Hyndman's CRAN demography package. My require further updating if HMD has since altered the codes.
ccodes <- read.csv('./HMD Country Codes.csv', stringsAsFactors = FALSE)

# Rather than examine all countries, choose only select examples from around the world
countriesToExamine <- c('USA', 'SWE', 'AUS', 'JPN', 'RUS', 'ITA','CHL')
fullCountryNames <- c('United States', 'Sweden', 'Australia', 'Japan', 'Russia', 'Italy', 'Chile')
names(fullCountryNames) <- countriesToExamine

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

countryShapes <- c(1:4,6:8)
names(countryShapes) <- countriesToExamine


# This line of code will examine all hmd countries: countriesToExamine <- ccodes$code

# A handy function to get factor "levels" into strings.
deLevel <- function(v){
  levels(v)[v]
}

# Figure 1 - Period Sex Ratio Curve for U.S. 2010 period lifetable
tSize <- 1.2
temp <- perFits$USA$origDat
plotDat <- temp[temp$Year==2010,]
srcPlot <- ggplot(plotDat, aes(x=age, y=lx.sr)) + geom_line() + xlab(label='Age') + ylab(label='Sex ratio') + geom_hline(yintercept=1, linetype=3) + geom_vline(xintercept=56, linetype=3) + scale_y_continuous(limits=c(.3,1.1), breaks=seq(.4,1.2,.2)) + annotate('text', x=54.5, y=.6, label='Sex ratio crossover: 56', angle=90, size=tSize) + annotate('text', x=10, y=1.07, label='Secondary sex ratio: 1.05', size=tSize)
writePlotAsTiff(srcPlot, 'USA2010SRC')

# A figure showing the excess male mortality, and cumulative excess
# Approximate mu_x with m_x
usa2010 <- perdat$USA$lifetabs[perdat$USA$lifetabs$Year==2010,c('sex','Age','mx')]

u10m <- usa2010[usa2010$sex=='m',]
u10f <- usa2010[usa2010$sex=='f',]
colnames(u10m)[3] <- 'm.mx'
colnames(u10f)[3] <- 'f.mx'
usa2010.2 <- merge(u10m[,c('Age','m.mx')], u10f[,c('Age','f.mx')])
# Get numeric age, and order data
usa2010.2$age <- as.numeric(deLevel(usa2010.2$Age))
usa2010.2 <- usa2010.2[order(usa2010.2$age),]

# Figure 2 - male vs. female mx
plotDat <- melt(usa2010.2[,c('age','m.mx','f.mx')], 'age')
plotDat2 <- plotDat[plotDat$age <= 60 & !is.na(plotDat$age),]
plotDat2$variable <- deLevel(plotDat2$variable)
plotDat2$Sex <- ifelse(plotDat2$variable=='m.mx', 'Male', 'Female')

mxBySexPlot <- ggplot(plotDat2, aes(x=age, y=value, linetype=Sex, colour=Sex)) + geom_line() + ylab(label='Mortality rate m(x)') + xlab(label='Age') + scale_linetype_manual(values=1:2) + scale_color_manual(values=per_coh_colours)
writePlotAsTiff(mxBySexPlot, 'fMx by sex USA 2010')


# Figure 3 - DMR and CDMR
# Calculate the excess and cumulative excess male mortality
usa2010.2$dmr  <- usa2010.2$m.mx-usa2010.2$f.mx
usa2010.2$cdmr <- cumsum(usa2010.2$dmr)
plotDat <- melt(usa2010.2[usa2010.2$age <= 60,c('age','dmr','cdmr')], 'age')
plotDat$variable <- deLevel(plotDat$variable)
plotDat$Variable <- ifelse(plotDat$variable=='dmr', 'DMR', 'Cumulative DMR')
plotDat$Metric <- plotDat$Variable

# Get the log secondary sex ratio. 
us10SSR <- perdat$USA$SSRs$SSR[perdat$USA$SSRs$Year==2010]
log_us10SSR <- log(us10SSR)

tSize <- 2
cmdrPlot <- ggplot(plotDat, aes(x=age, y=value, linetype=Metric, colour=Metric)) + geom_line() + ylab(label='Difference in m(x)') + xlab(label='Age') + scale_linetype_manual(values=1:2) + geom_hline(yintercept=log_us10SSR, linetype=3) + geom_vline(xintercept=55, linetype=3) + annotate('text', x=53.5, y=.02, label='Sex ratio crossover: 55', angle=90, size=tSize) + annotate('text', x=20, y=.051, label='Natural log of secondary sex ratio: 0.047', size=tSize) + scale_color_manual(values=per_coh_colours)
writePlotAsTiff(cmdrPlot, 'cmdr Plot')

perFullCrossDat <- rBindThisList(lapply(perFits, function(x0) x0$allCrossPoints))
cohFullCrossDat <- rBindThisList(lapply(cohFits, function(x0) x0$allCrossPoints))


# Figures 4 and 5.
perPlotDat <- perFullCrossDat[perFullCrossDat$year >= startYear,]
cohPlotDat <- cohFullCrossDat[cohFullCrossDat$year >= startYear,]

plotCountries <- function(plotDat, type='Period'){
  plotDat$country <- fullCountryNames[plotDat$country]
  legNames0 <- unique(plotDat$country)
  legNames <- legNames0[order(legNames0)]
  plotDat$Country <- factor(plotDat$country, levels=legNames)
  
  cShapes2 <- countryShapes
  names(cShapes2) <- fullCountryNames[names(cShapes2)]
  
  ggplot(plotDat, aes(x=year, y=ageCross, shape=Country, colour=Country)) + geom_jitter(size=shapeSize) + xlab(label='Year') + ylab(label = 'Sex ratio crossover') + scale_shape_manual(values=cShapes2[legNames]) + scale_color_manual(values=country_color_map[legNames])
}

# Plot period SRX - Figure 4
perPlots <- plotCountries(perPlotDat)
# Plot cohort SRX - Figure 5
cohPlots <- plotCountries(cohPlotDat, type='Cohort')

writePlotAsTiff(perPlots, 'Period SRX')
writePlotAsTiff(cohPlots, 'Cohort SRX')


# Period vs. Cohort - Figure 6
perPlotDat$type <- 'Period'
cohPlotDat$type <- 'Cohort'
bigDat <- rbind(perPlotDat, cohPlotDat)
colnames(bigDat)[4] <- 'Type'
bigDat$Country <- fullCountryNames[bigDat$country]
countriesToPlot <- c('Australia', 'Italy', 'United States', 'Sweden')
pvcPlot <- ggplot(bigDat[bigDat$Country %in% countriesToPlot,], aes(x=year, y=ageCross, shape=Type, colour=Type)) + geom_jitter(size=shapeSize) + xlab(label='Year') + ylab(label = 'Sex ratio crossover') + facet_wrap(~Country, scales='fixed') + scale_shape_manual(values=c(1,4)) + scale_color_manual(values=per_coh_colours)
writePlotAsTiff(pvcPlot, 'Cohort vs Period')


# Descriptive Statistics

# # Basic descriptive statistics for SSRs
# allSSRs <- rBindThisList(lapply(perdat, function(x0) x0$SSRs))
# allSSRs <- allSSRs[allSSRs$Year >= 1850,]
# yearRangeList <- lapply(split(allSSRs, allSSRs$country), function(x0) range(x0$Year))
# cname <- names(yearRangeList)
# # Store in a table
# ssrDat <- data.frame(country=cname, matrix(unlist(yearRangeList), nrow=7, byrow=T))
# colnames(ssrDat)[2:3] <- c('minyear','maxyear')
# ssrDat$yearRange <- paste(ssrDat$minyear, ssrDat$maxyear, sep='-')
# ssrDat$minSSR <-  unlist(lapply(split(allSSRs, allSSRs$country), function(x0) min(x0$SSR)))
# ssrDat$maxSSR <-  unlist(lapply(split(allSSRs, allSSRs$country), function(x0) max(x0$SSR)))
# ssrDat$meanSSR <- unlist(lapply(split(allSSRs, allSSRs$country), function(x0) mean(x0$SSR)))
# ssrDat$stdev_100 <- unlist(lapply(split(allSSRs, allSSRs$country), function(x0) sd(x0$SSR)*100))
# # ssrDat$pearsons <-unlist(lapply(split(allSSRs, allSSRs$country), function(x0) cor(x0$Year, x0$SSR)))
# ssrDat <- ssrDat[,c('country','yearRange','minSSR','maxSSR','meanSSR','stdev_100','pearsons')]
# # Extend to the full sample
# allSSRDat <- ssrDat[1,]; allSSRDat$country <- 'All'
# allSSRDat$yearRange <- paste(min(allSSRs$Year), max(allSSRs$Year), sep='-')
# allSSRDat$minSSR <- min(allSSRs$SSR); allSSRDat$maxSSR <- max(allSSRs$SSR); allSSRDat$meanSSR <- mean(allSSRs$SSR)
# allSSRDat$stdev_100 <- sd(allSSRs$SSR)*100; allSSRDat$pearsons <- cor(allSSRs$Year, allSSRs$SSR)
# # Append them together
# print(ssrTab <- rbind(allSSRDat, ssrDat))

# Descriptive table for period sex ratio crossovers
allPerSRX <- perFullCrossDat[perFullCrossDat$year >= 1850,]
yearRangeList <- lapply(split(allPerSRX, allPerSRX$country), function(x0) range(x0$year))
cname <- names(yearRangeList)
psrxDat <- data.frame(country=cname, matrix(unlist(yearRangeList), nrow=7, byrow=T))
colnames(psrxDat)[2:3] <- c('minyear','maxyear')
psrxDat$yearRange <- paste(psrxDat$minyear, psrxDat$maxyear, sep='-')
psrxDat$minSRX <-  unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) min(x0$ageCross, na.rm=TRUE)))
psrxDat$maxSRX <-  unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) max(x0$ageCross, na.rm=TRUE)))
psrxDat$meanSRX <- unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) mean(x0$ageCross, na.rm=TRUE)))
psrxDat$stdev <- unlist(lapply(split(allPerSRX, allPerSRX$country), function(x0) sd(x0$ageCross, na.rm=TRUE)))

# Some SRX are missing, meaning that they never cross
# Number of missing SRX
missTabDat <- rBindThisList(lapply(split(allPerSRX, allPerSRX$country), function(x0) {
  
  
  missYears0 <- x0$year[is.na(x0$ageCross)]
  if (length(missYears0) > 0){
    stop('There was an NA age cross found!')
  }
  
  yearDiffs <- diff(x0$year)
  
  
  missYearsStr <- 'None'
  
  atLeastOneMissingYear <- any(yearDiffs > 1)
  if (atLeastOneMissingYear){
    print('d')
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
    # missingYears <- prevYears:postYears
    missYearsStr <- paste(missYears, collapse=', ')
  }
  
  if (x0$country[1]=='ITA'){
    print('d')
  }
  data.frame(country=x0$country[1], noCrossYears=missYearsStr)
}))
psrxDat <- merge(psrxDat, missTabDat)
psrxDat <- psrxDat[,c('country','yearRange','minSRX','maxSRX','meanSRX','stdev','noCrossYears')]

# Extend to the full sample
allPSRXDat <- psrxDat[1,]; allPSRXDat$country <- 'All'
allPSRXDat$yearRange <- paste(min(allPerSRX$year), max(allPerSRX$year), sep='-')
allPSRXDat$minSRX <- min(allPerSRX$ageCross, na.rm=TRUE); allPSRXDat$maxSRX <- max(allPerSRX$ageCross, na.rm=TRUE); allPSRXDat$meanSRX <- mean(allPerSRX$ageCross, na.rm=TRUE)
allPSRXDat$stdev <- sd(allPerSRX$ageCross, na.rm=TRUE)
allPSRXDat$noCrossYears <- NA
# Append them together
print(psrxTab <- rbind(allPSRXDat, psrxDat))
# hist(perFullCrossDat$ageCross, xlab='Period Lifetable Sex Ratio Crossovers', main='')




# Descriptive table for cohort sex ratio crossovers
allCohSRX <- cohFullCrossDat[cohFullCrossDat$year >= 1850,]
yearRangeList <- lapply(split(allCohSRX, allCohSRX$country), function(x0) range(x0$year))
cname <- names(yearRangeList)
csrxDat <- data.frame(country=cname, matrix(unlist(yearRangeList), nrow=length(cname), byrow=T))
colnames(csrxDat)[2:3] <- c('minyear','maxyear')
csrxDat$yearRange <- paste(csrxDat$minyear, csrxDat$maxyear, sep='-')
csrxDat$minSRX <-  unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) min(x0$ageCross, na.rm=TRUE)))
csrxDat$maxSRX <-  unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) max(x0$ageCross, na.rm=TRUE)))
csrxDat$meanSRX <- unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) mean(x0$ageCross, na.rm=TRUE)))
csrxDat$stdev <- unlist(lapply(split(allCohSRX, allCohSRX$country), function(x0) sd(x0$ageCross, na.rm=TRUE)))

csrxDat$country <- deLevel(csrxDat$country)

csrxDat <- csrxDat[order(csrxDat$country),]

# csrxDat <- merge(csrxDat, missTabDat)

csrxDat <- csrxDat[,c('country','yearRange','minSRX','maxSRX','meanSRX','stdev')]

# Extend to the full sample
allcsrxDat <- csrxDat[1,]; allcsrxDat$country <- 'All'
allcsrxDat$yearRange <- paste(min(allCohSRX$year), max(allCohSRX$year), sep='-')
allcsrxDat$minSRX <- min(allCohSRX$ageCross, na.rm=TRUE); allcsrxDat$maxSRX <- max(allCohSRX$ageCross, na.rm=TRUE); allcsrxDat$meanSRX <- mean(allCohSRX$ageCross, na.rm=TRUE)
allcsrxDat$stdev <- sd(allCohSRX$ageCross, na.rm=TRUE)
# allcsrxDat$noCrossYears <- NA
# Append them together

print(csrxTab <- rbind(allcsrxDat, csrxDat))
allSSRs <- rBindThisList(lapply(perdat, function(x0) x0$SSRs))
mean(allSSRs$SSR, na.rm = T)
sd(allSSRs$SSR, na.rm = T)
