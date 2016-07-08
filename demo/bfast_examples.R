# installing and loading packages
library(devtools)
library(bfast)
# installing and loading wtss.R
install_github("e-sensing/wtss.R")
library(wtss.R)

# create a connection using a serverUrl
chronos = wtss("http://www.dpi.inpe.br/mds/mds")

# Get the list of coverages provided by the service
coverages = listCoverages(chronos)

# Get the description of the third coverage
cv = describeCoverage(chronos,coverages[3])

# Get a time series
ts = timeSeries(chronos, names(cv), attributes="ndvi", latitude=-10.408, longitude=-53.495, start="2000-01-01", end="2016-04-15")

# plot the time-series in a zoo object
plot(ts$MOD13Q1$attributes$ndvi)

# time series in a ts object with all the original values
time_series_ts1 = ts(coredata(ts$MOD13Q1$attributes$ndvi), freq=365.25/(as.numeric(difftime(index(ts$MOD13Q1$attributes$ndvi[2]),index(ts$MOD13Q1$attributes$ndvi[1]),units = "days"))), start=decimal_date(ymd(index(ts$MOD13Q1$attributes$ndvi[1]))))
# using bfast for checking for one major break in the time series
bfast01_time_series_ts1 = bfast01(time_series_ts1)
# plot bfast result
plot(bfast01_time_series_ts1)

# time series in a ts object without not available values
time_series_ts2 = ts(coredata(ts$MOD13Q1$attributes$ndvi)[!is.na(coredata(ts$MOD13Q1$attributes$ndvi))],freq=365.25/(as.numeric(difftime(index(ts$MOD13Q1$attributes$ndvi[2]),index(ts$MOD13Q1$attributes$ndvi[1]),units="days"))),start=decimal_date(ymd(index(ts$MOD13Q1$attributes$ndvi)[1])))
# using bfast for an iterative break detection in seasonal and trend component of a time series
plot(bfast(time_series_ts2, max.iter=1))

# time series in a ts object with part of the original values
time_series_ts3 = ts(coredata(ts$MOD13Q1$attributes$ndvi[1:266]),freq=365.25/(as.numeric(difftime(index(ts$MOD13Q1$attributes$ndvi[2]),index(ts$MOD13Q1$attributes$ndvi[1]),units = "days"))),start=decimal_date(ymd(index(ts$MOD13Q1$attributes$ndvi[1]))))
# using bfast for monitoring disturbances in time series in near real-time
plot(bfastmonitor(time_series_ts3, start=time(time_series_ts3)[228], history=time(time_series_ts3)[1]))
