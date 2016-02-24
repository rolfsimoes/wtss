# R API for Web Time Series Service

WTSS is a lightweight web service for handling remote sensing imagery as time series. Through a simple and effective representation for time series, this web service can be easily integrated into free and open source tools such as R, Python and web browser.

The R API can be found at <a href="http://www.dpi.inpe.br/mds/mds/">http://www.dpi.inpe.br/mds/mds<a>.

This R API is an update version of the service provided by <a href="https://github.com/albhasan/rwtss">https://github.com/albhasan/rwtss<a>.

## Building and Installing the Dependencies

### Prerequisites:

- <a href="http://git-scm.com/">Git</a>.

- <a href="http://www.r-project.org/">R</a>.

- <a href="http://www.rstudio.com/">Rstudio</a>.

### Build Instructions

- Clone the project: <code>git clone https//github.com/e-sensing/wtss.R.git</code>.

- Open Rstudio, go to File - Open Project and pick the file <code>rwtss.Rproj</code>.

- Install the required packages <code>install.packages(c("roxygen2", "testthat"))</code>.

- Go to the <i>Build</i> tab in the upper-right panel and press the button <i>Build & Reload</i>. After this the package is ready to use.

- You can also create a source package: Go to the <i>Build</i> tab, display the menu <i>More</i> and select the option <i>Build Source Package</i>.

### Install dependencies, load the rwtss package and run a test:

- Open RStudio

- Install devtools <code>install.packages("devtools")</code>
 
- Load devtools <code>library(devtools)</code>
 
- Install the rwtss package <code>install_github("e-sensing/wtss.R")</code>

- Load the rwtss package <code>library(rwtss)</code>

- Create a connection <code>obj = wtss("http://www.dpi.inpe.br/mds/mds")</code>

- Get the list of products provided by the service <code>objlist = listCoverages(obj)</code>

- Get the description of an specific product <code>objdesc = describeCoverages(obj,"MOD09Q1")</code>

- Get a time series <code>ts1 = getTimeSeries(obj, coverages="MOD09Q1", datasets=c("nir","quality","red","evi2"), latitude=-12, longitude=-45, start="2004-01-01", end="2004-05-01")</code>

## Reporting Bugs

Any problem should be reported to esensing-developers@dpi.inpe.br.


For more information on wtss.R, please, visit its main web page at: http://www.dpi.inpe.br/esensing.
