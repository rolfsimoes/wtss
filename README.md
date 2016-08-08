# R Client API for Web Time Series Service

**wtss.R** is an R client package for handling Web Time-Series Service (WTSS) in the client side. For more information on WTSS see  its specification and documentation in the TWS [site](https://github.com/e-sensing/tws). 

This R Client API is based on the orginal version developed by Alber Sanchez at https://github.com/albhasan/rwtss.

## Prerequisites

- **<a href="http://git-scm.com/">Git</a>:** For acessing the source code.

- **<a href="http://www.r-project.org/">R</a>:** For building and using the wtss.R package.

- **<a href="https://www.latex-project.org/">Latex</a>:** For including features for the production of vignettes.

- **<a href="http://www.rstudio.com/">Rstudio</a>:** suggestion of IDE to be used as the development environment.

## Using the wtss.R Package

- Open RStudio

- Install devtools <code>install.packages("devtools")</code>
 
- Load devtools <code>library(devtools)</code>

- Install the wtss.R package <code>install_github("luizassis/wtss.R")</code>

- Load the wtss.R package <code>library(wtss.R)</code>

- Create a connection <code>ts_server = WTSS("http://www.dpi.inpe.br/ts/wtss")</code>

- Get the list of coverages provided by the service <code>coverages = listCoverages(ts_server)</code>

- Get the description of the second coverage <code>cv1 = describeCoverage(ts_server,coverages[2])</code>

- Get a time series <code>ts = timeSeries(ts_server, names(cv1), cv1[[1]]$attributes$name, -45,-12,"2014-01-01","2014-12-31")</code>

<b>NOTE:</b> For older R versions, it is also necessary to install the <i>digest</i> package.
 
## Building Instructions

- Clone the project: <code>git clone https//github.com/luizassis/wtss.R.git</code>.

- Open Rstudio, go to File - Open Project and pick the file <code>wtss.R.Rproj</code>.

- Install the required packages <code>install.packages(c("roxygen2", "testthat"))</code>.

- Go to the <i>Build</i> tab in the upper-right panel and press the button <i>Build & Reload</i>. After this the package is ready to use.

- You can also create a source package: Go to the <i>Build</i> tab, display the menu <i>More</i> and select the option <i>Build Source Package</i>.


## Reporting Bugs

Any problem should be reported to luizffga@dpi.inpe.br.


<!--For more information on wtss.R, please, visit its main web page at: http://www.dpi.inpe.br/esensing.-->
