# R Client API for Web Time Series Service

**wtss** is an R client package for handling Web Time-Series Service (WTSS) in the client side. For more information on WTSS see  its specification and documentation in the EOWS [site](https://github.com/e-sensing/eows). 

This R Client API is based on the orginal version developed by Alber Sanchez at https://github.com/albhasan/rwtss.

## Getting started

Installing and loading wtss package

``` r
devtools::install_github("e-sensing/wtss") # github repository name is wtss.R
library(wtss) # R package name is wtss
```

A simple example of creating a WTSS connection

``` r 
ts.server <- WTSS("http://www.dpi.inpe.br/tws/wtss")
```

The result is a Object of Class WTSS. 

``` r
ts.server
```

``` r
## Object of Class WTSS

## serverUrl:  http://www.dpi.inpe.br/tws/wtss 
## listCoverages: itobi merge mixl8mod mixl8mod_f mod13q1_512
```

It is possible to get the list of coverages provided by the service.

``` r
coverages <- listCoverages(ts.server)
```

The object is a vector containing all the coverages provided by the service. 

``` r
coverages
```

``` r
## [1] "itobi"       "merge"     "mixl8mod"     "mixl8mod_f"     "mod13q1_512"
```

After that, we are able to acquire the coverage metadata. This function returns a named list of the coverage containing its attributes. In the example below, we can see how to get metadata from the second coverage.

```r
coverage.name <- "mod13q1_512"
cv <- describeCoverage(ts.server, coverage.name)
```

Finally, users can get the time series based on a set of required parameters.

```r
attributes <- cv[[names(cv)]]$attributes$name

long <- -53.495
lat <- -10.408

start <- "2000-02-18"
end <- "2017-02-18"
  
ts = timeSeries(object = ts.server, 
                coverages = coverage.name, 
                attributes = attributes, 
                latitude = lat, 
                longitude = long, 
                start = start, 
                end = end)
```

Plot the time series 

```r
plot(ts[[1]]$attributes[,1], main=sprintf("Pixel Center Coordinates Time-Series (%5.3f, %5.3f)", ts[[1]]$center_coordinate$latitude, ts[[1]]$center_coordinate$longitude), xlab="Time", ylab="Normalized Difference Vegetation Index")
```

<p align="center">
<img src="inst/extdata/plot-ts-timeseries.png" alt="Figure 1 - Vegetation index (ts time series)."  />
<p class="caption" align="center">
Figure 1 - Vegetation index (ts time series).
</p>
</p>

## References

VINHAS, L.; QUEIROZ, G. R.; FERREIRA, K. R.; CÂMARA, G. [Web Services for Big Earth Observation Data](http://urlib.net/8JMKD3MGP3W34P/3N2U9JL). In: BRAZILIAN SYMPOSIUM ON GEOINFORMATICS, 17. (GEOINFO), 2016, Campos do Jordão, SP. Proceedings... 2016.

## Reporting Bugs

Any problem should be reported to esensing-developers@dpi.inpe.br.
