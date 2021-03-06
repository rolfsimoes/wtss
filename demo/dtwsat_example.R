#-----------------------------------------------------------------
#--                                                             --
#--   (c) Adeline Maciel <adeline.maciel@inpe.br>               --
#--       Luiz Fernando Assis <luizffga@dpi.inpe.br>            --
#--       Raul Castanhari <raul.castanhari_funcate@inpe.br>     --
#--                                                             --
#--       Image Processing Division                             --
#--       National Institute for Space Research (INPE), Brazil  --
#--                                                             --
#--                                                             --
#--   dtwSat with wtss.R example - 2016-10-02                   --
#--                                                             --
#-----------------------------------------------------------------

# installing and loading packages
library(dtwSat)
library(wtss.R)

############################ Manipulate the time series

# create a connection using a WTSS class
server = WTSS("http://www.dpi.inpe.br/tws/wtss")

# get the list of coverages provided by the service
coverages = listCoverages(server)

# get the description of the second coverage
cv = describeCoverage(server, coverages[2])

# define attributes list
attr <- c("ndvi", "evi")

# get a time series
spatio_temporal = timeSeries(server, names(cv), attributes=attr, latitude=-11.62399, longitude=-56.2397, start="2000-02-18", end="2016-01-01")

############################ Prepocessing the time series for dtwSat patterns

# put ts and patterns into the same scale
spatio_temporal_ts1 <- spatio_temporal[[names(spatio_temporal)]]$attributes*0.0001

# remove ts attributes not presented in the first class pattern
keep = colnames(yearly_patterns_mt@timeseries[[1]])
spatio_temporal_ts1 = spatio_temporal_ts1[, names(spatio_temporal_ts1) %in% keep, drop=FALSE]
attr <- names(spatio_temporal_ts1)

# transform time series into twdtwTimeSeries if any attribute matches
if(!is.null(attr)) {
  ts = twdtwTimeSeries(spatio_temporal_ts1)
} else {
  stop("There is no attribute matching within time series with dtwSat pattern!!")
}

############################ Manipulate the patterns

# building patterns according to the attributes
patterns <- lapply(yearly_patterns_mt@timeseries, function(p) {
  object_zoo <- vector("list")
  dates <- index(p)
  for(j in 1:length(attr))
    object_zoo[[j]] <- as.vector(coredata(p[,c(j)]))
  names(object_zoo) <- attr
  as.zoo(as.data.frame(object_zoo), dates)
})

names(patterns) <- names(yearly_patterns_mt@timeseries)

# transforming patterns into twdtw Time Series
patt = twdtwTimeSeries(patterns)

# Applying TWDTW Analysis
log_fun = logisticWeight(alpha=-0.1, beta=100) 
matches = twdtwApply(x=ts, y=patt, weight.fun=log_fun, keep=TRUE)  

# plot the alignments
plot(x = matches, type = "classification", overlap=0.5)
