# installing and loading packages
library(bfast)
library(wtss.R)

# create a connection using a serverUrl
chronos = WTSS("http://www.dpi.inpe.br/tws/wtss")

# Get the list of coverages provided by the service
coverages = listCoverages(chronos)

# Get the description of the third coverage
cv = describeCoverage(chronos,coverages[2])

# Get a time series
spatio_temporal = timeSeries(chronos, names(cv), attributes=cv[[names(cv)]]$attributes$name[1], latitude=-10.408, longitude=-53.495, start="2000-02-18", end="2016-01-01")

# plot the time-series in a zoo object
plot(spatio_temporal[[names(cv)]]$attributes[,1])

# time series in a ts object with all the original values
time_series_ts1 = ts(coredata(spatio_temporal[[names(cv)]]$attributes[,1]), freq=365.25/(as.numeric(difftime(index(spatio_temporal[[names(cv)]]$attributes[2]),index(spatio_temporal[[names(cv)]]$attributes[1]),units = "days"))), start=decimal_date(ymd(index(spatio_temporal[[names(cv)]]$attributes[1]))))

# using bfast for checking for one major break in the time series
bfast01_time_series_ts1 = bfast01(time_series_ts1)

# plot bfast result
plot(bfast01_time_series_ts1)

# time series in a ts object without not available values
time_series_ts2 = ts(coredata(spatio_temporal[[names(cv)]]$attributes[,1])[!is.na(coredata(spatio_temporal[[names(cv)]]$attributes[,1]))], freq=365.25/(as.numeric(difftime(index(spatio_temporal[[names(cv)]]$attributes[2]),index(spatio_temporal[[names(cv)]]$attributes[1]),units = "days"))), start=decimal_date(ymd(index(spatio_temporal[[names(cv)]]$attributes[1]))))

# using bfast for an iterative break detection in seasonal and trend component of a time series
plot(bfast(time_series_ts2, max.iter=1))

# time series in a ts object with part of the original values
time_series_ts3 = ts(coredata(spatio_temporal[[names(cv)]]$attributes[,1])[1:270], freq=365.25/(as.numeric(difftime(index(spatio_temporal[[names(cv)]]$attributes[2]),index(spatio_temporal[[names(cv)]]$attributes[1]),units = "days"))), start=decimal_date(ymd(index(spatio_temporal[[names(cv)]]$attributes[1]))))

# using bfast for monitoring disturbances in time series in near real-time
plot(bfastmonitor(time_series_ts3, start=time(time_series_ts3)[228], history=time(time_series_ts3)[1]))
