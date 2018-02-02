


# Plot period SRX
perPlots <- plotCountries(perPlotDat)
# Plot cohort SRX
cohPlots <- plotCountries(cohPlotDat, type='Cohort')

writePlotAsTiff(perPlots, 'Period SRX')
writePlotAsTiff(cohPlots, 'Cohort SRX')