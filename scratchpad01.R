
# This function does the first sex ratio analyses for a given set of year-specific life-table data, formatted like the HMD lifetables read in from the hmd.periodLifeTab function. (Note, this requires a country name field). The lowerBound parameter determines the first year for which the regression analyses begin.
# Output includes: STILL WORKING ON THIS
srAnalyses <- function(ltdat, lowerBoundYear=1920){
  colsToKeep <- c('country','Year','Age','lx')
  f <- ltdat[ltdat$sex=='f', colsToKeep]
  colnames(f)[4] <- 'lx.f'
  m <- ltdat[ltdat$sex=='m', colsToKeep]
  colnames(m)[4] <- 'lx.m'
  d <- merge(f, m)
  SSR <- 1.05
  d$lx.sr <- SSR * d$lx.m / d$lx.f
  d$age <- as.numeric(deLevel(d$Age))
  # Keep only up through age 100
  d1 <- d[!(is.na(d$age)) & d$age <= 100,]
  origDat <- d1
  
  country <- d1$country[1]
  cName <- ccodes$country[ccodes$code==country]
  print(paste('\n--',country,'--\n'))
  
  # Plots of all the sex ratio curves.
  temp <- d1[d1$Year >= lowerBoundYear,]
  temp$year <- as.character(temp$Year)
  
  
  summaryCurve <- ggplot(temp[temp$Year %% 20 == 10 | temp$Year == 2000,], aes(x=age, y=lx.sr, colour=year)) + geom_line() + geom_hline(yintercept = 1, linetype=3) + labs(title=paste('Sex Ratio Curve\n',cName, 'Select Years'))
  
  allCurves <- lapply(split(temp, temp$Year), function(x0){
    ggplot(x0, aes(x=age, y=lx.sr)) + geom_line() + geom_hline(yintercept = 1, linetype=3) + labs(title=paste('Sex Ratio Curve\n',cName, x0$Year[1])) 
  })
  
  
  crossAgex <- as.data.frame(rbindlist(lapply(split(d1, d1$Year), function(x0){
    # Order the data points by age
    x <- x0[order(x0$age),]
    # Find every place the sex ratio is greater than 1, and store it as a 1.
    greaterOne <- (x$lx.sr > 1)*1
    # The sum of these 1's gives the number of age groups that are under age 1. Since they are single year of age data, and 
    # the first age groups is 0, just subtract 1 from the sum to get the necessary crossing age.
    ageCross <- sum(greaterOne)-1
    return(data.frame(year=x$Year[1], ageCross=ageCross))
  })))
  crossAgex$country <- country
  allCrossPoints <- crossAgex
  
  crossAgex <- allCrossPoints[allCrossPoints$year >= lowerBoundYear,]
  
  # Fit a linear model
  ageReg <- lm(ageCross ~ year, data=crossAgex)
  regSum <- summary(ageReg)
  linReg <- ageReg
  parms <- ageReg$coefficients
  
  # Place the fitted values in the dataset
  crossAgex$linFit <- parms[1] + crossAgex$year*parms[2]
  
  # A datset for storing key model info
  fitParms <- data.frame(country, linInt=parms[1], linBeta=parms[2], linR2=regSum$r.squared, stringsAsFactors = FALSE)
  
  # Fit an exponential model
  ageReg <- lm(log(ageCross) ~ year, data=crossAgex)
  regSum <- summary(ageReg)
  expReg <- ageReg
  parms <- ageReg$coefficients
  
  # Place the fitted values in the dataset
  crossAgex$expFit <- exp(parms[1] + crossAgex$year*parms[2])
  
  # key model info
  fitParms$expInt <- parms[1]; fitParms$expBeta <- parms[2]; fitParms$expR2=regSum$r.squared;
  rownames(fitParms) <- NULL
  
  
  # Make a plot
  basePlot <- ggplot(allCrossPoints, aes(x=year, y=ageCross)) + geom_point() + labs(title=paste('Sex Ratio Crossover by Year Period Life Tables\n',cName))
  linPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=linFit)) + labs(title=paste('Sex Ratio Crossover Linear Fit Period Life Tables\n',cName))
  expPlot  <- ggplot(crossAgex, aes(x=year, y=ageCross)) + geom_point() + geom_line(data=crossAgex, aes(x=year, y=expFit)) + labs(title=paste('Sex Ratio Crossover Exponential Fit Period Life Tables\n',cName))
  
  # Return objects of interest
  obj <- list(country, SSR, crossAgex, fitParms, linReg, expReg, basePlot, linPlot, expPlot, allCurves, summaryCurve, origDat)
  names(obj) <- c('country', 'SSR', 'crossAgex', 'fitParms', 'linReg', 'expReg', 'basePlot', 'linPlot', 'expPlot', 'allCurves', 'summaryCurve', 'origDat')
  obj
}

countryFits1 <- lapply(perlts, srAnalyses)

lapply(countryFits1, function(x0) x0$basePlot)
lapply(countryFits1, function(x0) x0$linPlot)
lapply(countryFits1, function(x0) x0$expPlot)