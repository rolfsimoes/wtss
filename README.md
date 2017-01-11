# R Client API for Web Time Series Service

**wtss.R** is an R client package for handling Web Time-Series Service (WTSS) in the client side. For more information on WTSS see  its specification and documentation in the TWS [site](https://github.com/e-sensing/tws). 

This R Client API is based on the orginal version developed by Alber Sanchez at https://github.com/albhasan/rwtss.

## Getting started

Installing and loading wtss.R package

``` r
devtools::install_github("luizassis/wtss.R")
library(wtss.R)
```

A simple example

``` r
# create a WTSS connection 
ts_server = WTSS("http://www.dpi.inpe.br/tws/wtss")

# get the list of coverages provided by the service 
coverages = listCoverages(ts_server)

# get the description of the second coverage 
cv = describeCoverage(ts_server,coverages[2])

# get a time series 
ts = timeSeries(ts_server, names(cv), cv[[1]]$attributes$name, latitude=-10.408, longitude=-53.495, start="2000-02-18", end="2016-01-01")

plot(ts[[1]]$attributes[,1], main=sprintf("Pixel Center Coordinates Time-Series (%5.3f, %5.3f)", ts[[1]]$center_coordinate$latitude, ts[[1]]$center_coordinate$longitude), xlab="Time", ylab="Normalized Difference Vegetation Index")
```

<p align="center">
<img src="images/plot-ts-timeseries.png" alt="Figure 1 - Vegetation index (ts time series)."  />
<p class="caption" align="center">
Figure 1 - Vegetation index (ts time series).
</p>
</p>

## References

G. R. de Queiroz, K. R. Ferreira, L. Vinhas, G. Camara, R. W. da Costa, R. C. M. de Souza, V. W. Maus, and A. Sanchez. WTSS: um serviço web para extração de séries temporais de imagens de sensoriamento remoto. In Proceeding of the XVII Remote Sensing Brazilian Symposium, pages 7553-7560, 2015.

## Reporting Bugs

Any problem should be reported to luizffga@dpi.inpe.br.
