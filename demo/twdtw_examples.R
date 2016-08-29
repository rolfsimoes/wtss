# installing and loading packages
library(dtwSat)
library(wtss.R)

# create a connection using a serverUrl
chronos = WTSS("http://www.dpi.inpe.br/tws/wtss")

# get the list of coverages provided by the service
coverages = listCoverages(chronos)

# get the description of the third coverage
cv = describeCoverage(chronos,coverages[2])

# define attributes list
attr <- c("ndvi", "evi",  "red",  "nir",  "blue", "mir" )

# get a time series
spatio_temporal = timeSeries(chronos, names(cv), attributes=attr, latitude=-10.408, longitude=-53.495, start="2000-02-18", end="2016-01-01")

# plot the time-series in a zoo object
ts = twdtwTimeSeries(spatio_temporal[[names(spatio_temporal)]]$attributes)
plot(ts, type="timeseries")

# in the region where the time series was observed we have soybean, cotton, and maize, whose typical temporal pattern
patt = twdtwTimeSeries(patterns.list)

# using the the temporal patterns to run the TWDTW analysis
log_fun = logisticWeight(alpha=-0.1, beta=100) # Logistic time-weight
matches = twdtwApply(x=ts, y=patt, weight.fun=log_fun, keep=TRUE) 

# plot the alignments
plot(x = matches, type = "alignments")
