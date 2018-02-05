# Number of missing SRX
missTabDat <- rBindThisList(lapply(split(allCohSRX, allCohSRX$country), function(x0) {
  
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
  
  data.frame(country=x0$country[1], noCrossYears=missYearsStr)
}))


csrxDat <- merge(csrxDat, missTabDat)

csrxDat <- csrxDat[,c('country','yearRange','minSRX','maxSRX','meanSRX','stdev','noCrossYears')]

# Extend to the full sample
allcsrxDat <- csrxDat[1,]; allcsrxDat$country <- 'All'
allcsrxDat$yearRange <- paste(min(allCohSRX$year), max(allCohSRX$year), sep='-')
allcsrxDat$minSRX <- min(allCohSRX$ageCross, na.rm=TRUE); allcsrxDat$maxSRX <- max(allCohSRX$ageCross, na.rm=TRUE); allcsrxDat$meanSRX <- mean(allCohSRX$ageCross, na.rm=TRUE)
allcsrxDat$stdev <- sd(allCohSRX$ageCross, na.rm=TRUE)
allcsrxDat$noCrossYears <- NA
# Append them together
print(csrxTab <- rbind(allcsrxDat, csrxDat))
