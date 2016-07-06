#' The wtss class
#'
#' Use this class for representing a client of a WTSS
#'
#'@section Slots :
#' \describe{
#' \item{\code{serverUrl}:}{Object of class \code{"character"}, URL of the server.}
#' \item{\code{exploreArrays}:}{List of arrays \code{"character"}.}
#' }
#'
#' @note No notes
#' @name wtss
#' @aliases wtss-class
#' @exportClass wtss
#' @author Victor Maus, Alber Sanchez, Luiz Fernando Assis, Gilberto Ribeiro
#' @import jsonlite
#' @import RCurl
#' @import methods
#' @import roxygen2
#' @import testthat
#' @import zoo
#' @import lubridate
setClass (
  
  # Set the name for the class
  Class = "wtss",
  
  # Define the slots
  slots = c(
    serverUrl = "character",
    exploreArrays = "character"
  ),
  
  # Set the default values for the slots.
  prototype=list(),
  
  # Test if the data is consistent. Only called if no initialize function is defined.
  validity = function(object)
  {
    if(length(object@serverUrl) != 1){
      stop ("[wtss: validation] Invalid server URL.")
    }
    
    if(nchar(object@serverUrl) <= 1){
      stop ("[wtss: validation] Invalid server URL.")
    }
    
#     if(length(object@exploreArrays) <= 1){
#       stop ("[wtss: validation] Invalid server URL.")
#     }
#     
#     cat("nchar(object@exploreArrays = ",nchar(object@exploreArrays))
#     if(nchar(object@exploreArrays) <= 1){
#       stop ("[wtss: validation] Invalid server URL.")
#     }
    
    return(TRUE)
  }
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod (
  
  # initialize function
  f="initialize",
  
  # Method signature
  signature="wtss",
  
  # Function definition
  definition=function(.Object, serverUrl)
  {
    
    if(!missing(serverUrl))
    {
      .Object@serverUrl <- serverUrl
      exploreArrays <- .listCoverages(.Object)
       if(class(exploreArrays) == "try-error")
         stop(exploreArrays)
       else
       .Object@exploreArrays <- exploreArrays
      #.Object@exploreArrays <- .listCoverages(.Object)
      validObject(.Object)
    }else{
      .Object@serverUrl <- character(0)
    }
    return(.Object)
  }
)

#' Creates a wtss object
#'
#' @param serverUrl A server URL
#' @rdname wtss
#' @docType methods
#' @export
#' @examples
#' #obj = wtss("http://www.dpi.inpe.br/ts/wtss")
wtss <- function(serverUrl)
{
  new (Class="wtss",serverUrl = serverUrl)
}

#*******************************************************
#SHOW
#*******************************************************
setMethod (
  
  # Name of the function
  f = "show", 
  
  # Method signature
  signature = "wtss", 
  
  # Stylish print of the objects
  definition=function(object)
  {
    
    # initial message
    cat(paste("Object of Class wtss\n\n"))
    
    # serverUrl
    cat(paste("serverUrl: ",paste(object@serverUrl),  "\n"))
    
    # exploreArrays
    cat("exploreArrays: ")
    cat(paste(object@exploreArrays), " ")
    
    return(invisible())
  }
)
setGeneric(name="show", def=function(object){standardGeneric("show")})

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
          function(object)
          {
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
#' #obj = wtss("http://www.dpi.inpe.br/ts/wtss")
#' #objlist = listCoverages(obj)
setGeneric("listCoverages",function(object){standardGeneric ("listCoverages")})

#' @rdname  listCoverages
setMethod("listCoverages","wtss",
          function(object)
          {
            .listCoverages(object) 
          }
)

.listCoverages <- function(object)
{
  
  url <- getServerUrl(object)
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  if (length(url) == 1 && nchar(url) > 1)
  {
    
    # concat list_coverages to the service URL 
    request <- paste(url,"list_coverages",sep="")
    
    # try only 10 times (avoid time out connection)
    while(class(items) == "try-error" & ce < 10) 
    {
      items <- .parseJSON(.sendRequest(request))
      ce <- ce + 1
    }
    
    # if the server does not answer any item
    if (class(items) == "try-error")
      return(items)
      
    # if the server answers correctly
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
#' #obj = wtss("http://www.dpi.inpe.br/ts/wtss")
#' #objdesc = describeCoverage(obj,"hotspot_monthly")
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
  
  if( length(url) == 1 && nchar(url) > 1 )
  {
    out <- lapply(coverages, function(cov)
          {
        
            # concat describe_coverage according to a name into the service URL 
            request <- paste(url,"describe_coverage?name=", cov, sep="")
            
            # try only 10 times (avoid time out connection)
            while(class(items) == "try-error" & ce < 10) {
              items <- .parseJSON(.sendRequest(request))
              ce <- ce + 1
            }
            
            # if the server does not answer any item
            if (class(items) == "try-error")
              return(items)
            
            return(items$attributes)
        })
    
    names(out) <- coverages
    
    return(out)
  
  }
  
}

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
#' #obj = wtss("http://www.dpi.inpe.br/ts/wtss")
#' #objlist = listCoverages(obj)
#' #objdesc = describeCoverage(obj,objlist[2])
#' #coordinates = list(c(-45,-12),  c(-54,-11))
#' #tsList = listTimeSeries(obj, names(objdesc), objdesc[[1]]$name, coordinates, "2004-01", "2004-05")
setGeneric("listTimeSeries",function(object,coverages,attributes,coordinates,start,end){standardGeneric("listTimeSeries")})

#' @rdname  listTimeSeries
setMethod("listTimeSeries","wtss",
          function(object,coverages,attributes,coordinates,start,end)
          {
            # check type of the list of coordinates 
            if( is.data.frame(coordinates) | is.matrix(coordinates))
              coordinates <- lapply(1:dim(coordinates)[1], function(i) coordinates[i,])
            
            if(!is.list(coordinates))
              stop("Missing a list. Please insert a list of longitude latitude coordinates in WGS84 coordinate system.")
            
            out <- lapply(coordinates, function(coords)
            {
              longitude <- coords[1]
              latitude <- coords[2]
              items <- .timeSeries(object,coverages,attributes,longitude,latitude,start,end)
            })
            
            return(out)
            
          }
)

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
#' #obj = wtss("http://www.dpi.inpe.br/ts/wtss")
#' #objlist = listCoverages(obj)
#' #objdesc = describeCoverages(obj,objlist)
#' #ts = timeSeries(obj, names(objdesc), objdesc[[1]]$name, -45,-12,"2004-01-01","2004-05-01")
setGeneric("timeSeries",function(object,coverages,attributes,longitude,latitude,start,end){standardGeneric("timeSeries")})

#' @rdname  timeSeries
setMethod("timeSeries","wtss",
          function(object,coverages,attributes,longitude,latitude,start,end)
          {
          
            .timeSeries(object,coverages,attributes,longitude,latitude,start,end)
            
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
  
  if(length(url) == 1 && nchar(url) > 1)
  {
    
    if(is.list(coverages))
    {
      
      out <- lapply(names(coverages), function(cov)
             {
                attributes <- coverages[[cov]]
                
                request <- paste(url,"time_series?coverage=",cov,"&attributes=",paste(attributes, collapse=","),
                                 "&latitude=",latitude,"&longitude=",longitude,
                                 "&start=",start,"&end=",end,sep="")
                
                # try only 10 times (avoid time out connection)
                while(class(items) == "try-error" & ce < 10) 
                {
                  items <- .parseJSON(.sendRequest(request))#items <- try(fromJSON(try(getURL(request))))
                  ce <- ce + 1
                }
                
                # if the server does not answer any item
                if (class(items) == "try-error")
                {
                  stop("\n Server connection timeout. Verify the URL or try again later.")
                  return(items)
                }
                
                # time series processing method call 
                timeseries <- .timeSeriesProcessing(items)
                
                return(timeseries)
                
              })
      
      names(out) <- names(coverages)
      
      return(out)
      
    } 
    else 
        if( is.character(coverages) && length(coverages)==1 && is.character(attributes)) 
        {
          
            request <- paste(url,"time_series?coverage=",coverages,"&attributes=",paste(attributes, collapse=","),
                             "&latitude=",latitude,"&longitude=",longitude,
                             "&start=",start,"&end=",end,sep="")
            
            # try only 10 times (avoid time out connection)
            while(class(items) == "try-error" & ce < 10) 
            {
              items <- .parseJSON(.sendRequest(request))
              ce <- ce + 1
            }
      
            # if the server does not answer any item
            if (class(items) == "try-error")
            {
              stop("\n Server connection timeout. Verify the URL or try again later.")
              return(items)
            }
            
            out <- list(.timeSeriesProcessing(items))
            
            names(out) <- coverages
            
            return(out)
        } 
        else 
        {
            stop("Missing either a list of coverages and attributes such as retrieved by describe_coverage()
           or a character with the coverage name and a character vector of dataset names.")
        }
  }
  
  return(NULL)
  
}

.timeSeriesProcessing <- function(items)
{
  attributes_list <- list(items$result$attributes)
  
  attributes.processed <- lapply(attributes_list, function(subdataset)
                          {
                              # assign attribute values 
                              value <- subdataset$values
                              
                              # assign corresponding missing values to the values vector
                              #value[value==subdataset$missing_value] <- NA
                              
                              # if scale factor (e.g., 10000) is not null, it is necessary to divide by it
                              #if( !is.null(subdataset$scale_factor) )
                              #  value <- value / as.numeric(subdataset$scale_factor)
                              
                              # assign values to dataframe
                              value <- data.frame(value, stringsAsFactors = FALSE)
                              
                              # dataset names to the values vectors 
                              names(value) <- subdataset$attribute

                              return(value)
                              
                          })
  
  attributes.processed <- data.frame(attributes.processed, stringsAsFactors = FALSE)
  
  # convert string into date format
  timeline <- unlist(strsplit(items$result$timeline, split=" "))
  
  # check date format
  format <- guess_formats(timeline[1], c("%Y-%m-%d", "%Y-%m"))
  
  # if monthly date
  if(format == "%Y-%m")
      timeline = as.Date(as.yearmon(timeline))
  else # if weekly date
      if(format == "%Y-%m-%d")
        timeline = as.Date(timeline, format)

  return(list(center_coordinate = data.frame(longitude=items$result$center_coordinate$longitude, latitude=items$result$center_coordinate$latitude), 
               attributes = zoo(attributes.processed, timeline))
  )
}

.sendRequest <- function(request)
{
  
  # check if URL exists
  if (url.exists(request))
    response <- getURL(request)
  else
  {
    # no valid http page error handling
    response <- "ERROR: Request to the web time series service failed. The URL server may be incorrect or the service does not exist."
    class(response) <- "try-error"
  }

    return(response)
      
}

.parseJSON <- function(response)
{
  
  # validate json
  if (validate(response))
    json_response <- fromJSON(response)
  else
  {
    # no json returned error handling
    json_response <- toJSON("ERROR: Request to the web time series service failed. The service may be down!")
    class(json_response) <- "try-error"
  }

  return(json_response)
}