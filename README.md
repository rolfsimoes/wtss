# R Client API for Web Time Series Service

WTSS is a lightweight web service for handling remote sensing imagery as time series. Through a simple and effective representation for time series, this web service can be easily integrated into free and open source tools such as R, Python and web browser (through JavaScript).

A JavaScript client can be found at <a href="http://github.com/gqueiroz/wtss/">http://github.com/gqueiroz/wtss<a>

## Building and Installing the Dependencies

### Prerequisites:

- Internet access.

- <a href="http://git-scm.com/">Git</a>.

- <a href="http://www.r-project.org/">R</a>.

- <a href="http://www.rstudio.com/">Rstudio</a>.

### To use the package:

- Open RStudio
- 
- Install devtools <code>install.packages("devtools")</code>
- 
- Load devtools <code>library(devtools)</code>
- 
- Install the rwtss package <code>install_github("e-sensing/wtss.R")</code>

### Usage examples:

- Load the rwtss package <code>library(rwtss)</code>

- Create a connection <code>obj = wtssClient("http://www.dpi.inpe.br/mds/mds")</code>

- Get the list of products provided by the service <code>objlist = listCoverages(obj)</code>
- 
- Get the description of an specific product <code>objdesc = describeCoverages(obj,"MOD09Q1")</code>

- Get a time series <code>ts1 = getTimeSeries(obj, coverages="MOD09Q1", datasets=c("nir","quality","red","evi2"), latitude=-12, longitude=-45, from="2004-01-01", to="2004-05-01")</code>

### Build Instructions

- Clone the project: <code>git clone https//github.com/e-sensing/wtss.R.git</code>.

- Open Rstudio, go to File - Open Project and pick the file <code>rwtss.Rproj</code>.

- Install the required packages <code>install.packages(c("roxygen2", "testthat"))</code>.

- Go to the <i>Build</i> tab in the upper-right panel and press the button <i>Build & Reload</i>. After this the package is ready to use.

- You can also create a source package: Go to the <i>Build</i> tab, display the menu <i>More</i> and select the option <i>Build Source Package</i>.
