missTabDat <- rBindThisList(lapply(split(allCohSRX, allCohSRX$country), function(x0) {
  missYears0 <- x0$year[is.na(x0$ageCross)]

  missYearsStr <- paste(missYears0, collapse=' ')
  if (trimws(missYearsStr)=='') {
    missYearsStr <-  'none'
  }
  data.frame(country=x0$country[1], missYears=missYearsStr)
}))

missTabDat
