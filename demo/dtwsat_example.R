#-----------------------------------------------------------------
#--                                                             --
#--   (c) Adeline Maciel <adeline.maciel@inpe.br>               --
#--       Luiz Fernando Assis <luizffga@dpi.inpe.br>            --
#--                                                             --
#--       Image Processing Division                             --
#--       National Institute for Space Research (INPE), Brazil  --
#--                                                             --
#--                                                             --
#--   dtwSat with wtss.R example - 2016-09-26                   --
#--                                                             --
#-----------------------------------------------------------------

# installing and loading packages
library(dtwSat)
library(wtss.R)

# create a connection using a WTSS class
server = WTSS("http://www.dpi.inpe.br/tws/wtss")

# get the list of coverages provided by the service
coverages = listCoverages(server)

# get the description of the second coverage
cv = describeCoverage(server, coverages[2])

# define attributes list
attr <- c("ndvi", "evi",  "red",  "nir",  "blue", "mir" )

# get a time series
spatio_temporal = timeSeries(server, names(cv), attributes=attr, latitude=-11.62399, longitude=-56.2397, start="2000-02-18", end="2016-01-01")

# put ts and signatures into the same scale
spatio_temporal_ts1 <- spatio_temporal[[names(spatio_temporal)]]$attributes*0.0001

# transform time series into twdtwTimeSeries
ts = twdtwTimeSeries(spatio_temporal_ts1)

# applying TWDTW analysis:
log_fun = logisticWeight(alpha=-0.1, beta=50) # Logistic time-weight
matches = twdtwApply(x=ts, y=yearly_patterns_mt, weight.fun=log_fun, keep=TRUE) 

# plot the alignments
plot(x = matches, type = "alignments")
