#' The wtss class
#'
#' Use this class for representing a client of a WTSS
#'
#'
#'@section Slots :
#' \describe{
#' \item{\code{serverUrl}:}{Object of class \code{"character"}, URL of the server.}
#' }
#'
#' @note No notes
#' @name wtss
#' @aliases wtss-class
#' @exportClass wtss
#' @author Victor Maus, Alber Sanchez, Luiz Fernando Assis, Gilberto Ribeiro
#' @import rjson
#' @import RCurl
#' @import methods
#' @import roxygen2
#' @import testthat
#' @import zoo
setClass (
  Class = "wtss",
  representation = representation(
    serverUrl = "character"
  ),
  validity = function(object){
    if(length(object@serverUrl) != 1){
      stop ("[wtss: validation] Invalid server URL.")
    }else{}
    if(nchar(object@serverUrl) <= 1){
      stop ("[wtss: validation] Invalid server URL.")
    }else{}
    return(TRUE)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod (
  f="initialize",
  signature="wtss",
  definition=function(.Object,serverUrl){
    if(!missing(serverUrl)){
      .Object@serverUrl <- serverUrl
      validObject(.Object)
    }else{
      .Object@serverUrl <- character(0)
    }
    return(.Object)
  }
)
#CONSTRUCTOR (USER FRIENDLY)
#' Creates a wtss object
#'
#' @param serverUrl A server URL
#' @rdname wtss
#' @docType methods
#' @export
#' @examples
#' #obj = wtss("http://www.dpi.inpe.br/mds/mds")
wtss <- function(serverUrl){
  new (Class="wtss",serverUrl = serverUrl)
}



#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object's server URL
#'
#' @param object A wtss object
#' @docType methods
#' @aliases getServerUrl-generic
#' @export
setGeneric("getServerUrl",function(object){standardGeneric ("getServerUrl")})

#' @rdname getServerUrl
setMethod("getServerUrl","wtss",
          function(object){
            if(substr(object@serverUrl,nchar(object@serverUrl),nchar(object@serverUrl))!="/")
              return(paste(object@serverUrl,"/",sep=""))
            return(object@serverUrl)
          }
)

#' Sets the object's server URL
#'
#' @param object A wtss object
#' @param aServerUrl A character representing the server URL
#' @docType methods
#' @export
setGeneric("setServerUrl",function(object, aServerUrl){standardGeneric ("setServerUrl")})

#' @rdname  setServerUrl
setMethod("setServerUrl","wtss",
          function(object, aServerUrl){
            object@serverUrl <- aServerUrl
          }
)

#' List coverages 
#'
#' @param object A wtss object
#' @docType methods
#' @export
#' @examples
#' #obj = wtss("http://www.dpi.inpe.br/mds/mds")
#' #objlist = listCoverages(obj)
setGeneric("listCoverages",function(object){standardGeneric ("listCoverages")})

#' @rdname  listCoverages
setMethod("listCoverages","wtss",
          function(object){
            .listCoverages(object) 
          }
)

.listCoverages <- function(object)
{
  url <- getServerUrl(object)
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  if( length(url) == 1 && nchar(url) > 1 ){
    #request <- paste(url,"list_coverages?output_format=json",sep="")
    request <- paste(url,"product_list?output_format=json",sep="")
    while(class(items) == "try-error" & ce < 10) {
      items <- .parseJSON(.sendRequest(request))#items <- try(fromJSON(try(getURL(request))))
      ce <- ce + 1
    }
    if (class(items) == "try-error"){
      stop("\n Server connection timeout. Verify the URL or try again later.")
      return(items)
    }
    return(unlist(items, use.names = FALSE))
  }
}

#' Describe coverage
#'
#' @param object A wtss object
#' @param coverages A character vector of coverage names
#' @docType methods
#' @export
#' @examples
#' #obj = wtss("http://www.dpi.inpe.br/mds/mds")
#' #objdesc = describeCoverage(obj,"MOD09Q1")
setGeneric("describeCoverage",function(object,coverages){standardGeneric("describeCoverage")})


#' @rdname  describeCoverage
setMethod("describeCoverage","wtss",
          function(object,coverages){
            .describeCoverage(object,coverages) 
          }
)

.describeCoverage <- function(object,coverages)
{
  url <- getServerUrl(object)
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  if( length(url) == 1 && nchar(url) > 1 ){
    out <- lapply(coverages, function(cov){
      #request <- paste(url,"describe_coverage?name=",cov,"&output_format=json",sep="")
      request <- paste(url,"dataset_list?product=",cov,"&output_format=json",sep="")
      while(class(items) == "try-error" & ce < 10) {
        items <- .parseJSON(.sendRequest(request))#items <- try(fromJSON(try(getURL(request))))
        ce <- ce + 1
      }
      if (class(items) == "try-error"){
        stop("\n Server connection timeout. Verify the URL or try again later.")
        return(items)
      }
      #return(items$attributes)
      return(items$datasets)
    })
    names(out) <- coverages
    return(out)
  }
}

#' Get time series
#'
#' @description This function retrieves the time series for a pair of coordinates.es
#' 
#' @param object Either a wtss object or a server URL
#' @param coverages Either a list of coverages and attributes such as retrieved by describe_coverage() or a character with the coverage name.
#' @param attributes A character vector of dataset names.
#' @param longitude A longitude in WGS84 coordinate system.
#' @param latitude A latitude in WGS84 coordinate system.
#' @param start A character with the start date in the format yyyy-mm-dd.
#' @param end A character with the end date in the format yyyy-mm-dd.
#' @docType methods
#' @export
#' @examples
#' #obj = wtss("http://www.dpi.inpe.br/mds/mds")
#' #objlist = listCoverages(obj)
#' #objdesc = describeCoverages(obj,objlist)
#' #tsAll = getTimeSeries(obj, objdesc,-45,-12,"2004-01-01","2004-05-01")
setGeneric("timeSeries",function(object,coverages,attributes,longitude,latitude,start,end){standardGeneric("timeSeries")})

#' @rdname  timeSeries
setMethod("timeSeries","wtss",
          function(object,coverages,attributes,longitude,latitude,start,end){
            .timeSeries(object,coverages,attributes,longitude,latitude,start,end)
          }
)

#' Get list of time series
#'
#' @description This function retrieves the time series for a list of coordinates.
#'
#' @param object Either a wtss object or a server URL
#' @param coverages Either a list of coverages and attributes such as retrieved by describe_coverage() or a character with the coverage name.
#' @param attributes A character vector of dataset names.
#' @param coordinates A list or data frame of longitude latitude coordinates in WGS84 coordinate system.
#' @param start A character with the start date in the format yyyy-mm-dd.
#' @param end A character with the end date in the format yyyy-mm-dd.
#' @docType methods
#' @export
#' @examples
#' #obj = wtss("http://www.dpi.inpe.br/mds/mds")
#' #objlist = listCoverages(obj)
#' #objdesc = describeCoverages(obj,objlist)
#' #coordinates = list( c(longitude=-45, latitude=-12),  c(longitude=-54, latitude=-11))
#' #tsAll = getListOfTimeSeries(obj, objdesc, coordinates, "2004-01-01", "2004-05-01")
setGeneric("getListOfTimeSeries",function(object,coverages,attributes,coordinates,start,end){standardGeneric("getListOfTimeSeries")})

#' @rdname  getListOfTimeSeries
setMethod("getListOfTimeSeries","wtss",
          function(object,coverages,attributes,coordinates,start,end){
            if( is.data.frame(coordinates) | is.matrix(coordinates))
              coordinates <- lapply(1:dim(coordinates)[1], function(i) coordinates[i,])
            if(!is.list(coordinates))
              stop("Missing a list. Please informe a list of longitude latitude coordinates in WGS84 coordinate system.")
            out <- lapply(coordinates, function(coords){
              longitude <- coords[1]
              latitude <- coords[2]
              items <- .timeSeries(object,coverages,attributes,longitude,latitude,start,end)
            })
            return(out)
          }
)


.timeSeries <- function(object,coverages,attributes,longitude,latitude,start,end)
{
  if(missing(object))
    stop("Missing either a wtss object or a server URL.")
  
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  url <- object
  
  if(class(object)=="wtss")
    url <- getServerUrl(object)
  
  if( length(url) == 1 && nchar(url) > 1 ){
    
    if(is.list(coverages)){
      out <- lapply(names(coverages), function(cov){
        attributes <- coverages[[cov]]
        request <- paste(url,"query?product=",cov,"&datasets=",paste(attributes, collapse=","),
                         "&latitude=",latitude,"&longitude=",longitude,
                         "&start=",start,"&end=",end,"&output_format=json",sep="")
        while(class(items) == "try-error" & ce < 10) {
          items <- .parseJSON(.sendRequest(request))#items <- try(fromJSON(try(getURL(request))))
          ce <- ce + 1
        }
        if (class(items) == "try-error"){
          stop("\n Server connection timeout. Verify the URL or try again later.")
          return(items)
        }
        timeseries <- .timeSeriesProcessing(items)
        return(timeseries)
      })
      names(out) <- names(coverages)
      return(out)
    } else if( is.character(coverages) && length(coverages)==1 && is.character(attributes)) {
      request <- paste(url,"query?product=",coverages,"&datasets=",paste(attributes, collapse=","),
                       "&latitude=",latitude,"&longitude=",longitude,
                       "&start=",start,"&end=",end,"&output_format=json",sep="")
      while(class(items) == "try-error" & ce < 10) {
        items <- .parseJSON(.sendRequest(request))#items <- try(fromJSON(try(getURL(request))))
        ce <- ce + 1
      }
      if (class(items) == "try-error"){
        stop("\n Server connection timeout. Verify the URL or try again later.")
        return(items)
      }
      out <- list(.timeSeriesProcessing(items))
      names(out) <- coverages
      return(out)
    } else {
      stop("Missing either a list of coverages and attributes such as retrieved by describe_coverage()
           or a character with the coverage name and a character vector of dataset names.")
    }
    }
  
  return(NULL)
  }

.timeSeriesProcessing <- function(items)
{
  attributes.processed <- lapply(items$result$datasets, function(subdataset)
  {
    
    value <- subdataset$values
    
    value[value==subdataset$missing_value] <- NA
    
    if( !is.null(subdataset$scale_factor) )
      value <- value / as.numeric(subdataset$scale_factor)
    
    value <- data.frame(value, stringsAsFactors = FALSE)
    
    names(value) <- subdataset$dataset
    
    return(value)
    
  })
  
  attributes.processed <- data.frame(attributes.processed, stringsAsFactors = FALSE)
  
  return( list(center_coordinate = data.frame(longitude=items$result$center_coordinate$longitude, latitude=items$result$center_coordinate$latitude), 
               attributes = zoo( attributes.processed, as.Date(items$result$timeline)))
  )
}

.sendRequest <- function(request){
  res = tryCatch({
    getURL(request)
  }, error = function(e) {
    stop("ERROR: An error occurred while retrieving data.")
  })
  return(res)
}

.parseJSON <- function(atext){
  res = tryCatch({
    fromJSON(atext)
  }, error = function(e) {
    stop(paste("ERROR: An error occurred while parsing JSON", e, sep = " - "))
  })
  return(res)
}