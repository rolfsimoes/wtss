#' The WTSS class
#'
#' Use this class for creating a connection to a Web Time Series Service (WTSS)
#'   
#'@section Slots :
#' \describe{
#' \item{\code{serverUrl}:}{Object of class \code{"character"}, URL of the server.}
#' \item{\code{listCoverages}:}{Object of class \code{"character"}, list of coverages.}
#' }
#'
#' @note No notes
#' @name WTSS
#' @aliases WTSS-class
#' @exportClass WTSS
#' @author Victor Maus, Alber Sanchez, Luiz Fernando Assis, Pedro Andrade, Gilberto Ribeiro
#' @import jsonlite
#' @import methods
#' @import RCurl
#' @import roxygen2
#' @import testthat
#' @import spacetime
#' @import sp
#' @import lubridate
#' @import zoo
setClass (
  
  # Set the name for the class
  Class = "WTSS",
  
  # Define the slots
  slots = c(
    serverUrl = "character",
    listCoverages = "character"
  ),
  
  # Set the default values for the slots.
  prototype=list(),
  
  # Test if the data is consistent. Only called if no initialize function is defined.
  validity = function(object)
  {
    if(length(object@serverUrl) != 1){
      stop ("[WTSS: validation] Invalid server URL.")
    }
    
    if(nchar(object@serverUrl) <= 1){
      stop ("[WTSS: validation] Invalid server URL.")
    }
    
#     if(length(object@exploreArrays) <= 1){
#       stop ("[WTSS: validation] Invalid server URL.")
#     }
#     
#     cat("nchar(object@exploreArrays = ",nchar(object@exploreArrays))
#     if(nchar(object@exploreArrays) <= 1){
#       stop ("[WTSS: validation] Invalid server URL.")
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
  signature="WTSS",
  
  # Function definition
  definition=function(.Object, serverUrl)
  {
    
    if(!missing(serverUrl))
    {
      .Object@serverUrl <- serverUrl
      arrays <- .listCoverages(.Object)
       if(class(arrays) == "try-error")
         stop(arrays)
       else
       .Object@listCoverages <- arrays
      #.Object@exploreArrays <- .listCoverages(.Object)
      validObject(.Object)
    }else{
      .Object@serverUrl <- character(0)
    }
    return(.Object)
  }
)

#' Creates a WTSS object
#'
#' @param serverUrl A server URL
#' @rdname WTSS
#' @docType methods
#' @export
#' @examples
#' obj = WTSS("http://www.dpi.inpe.br/ts/wtss")
WTSS <- function(serverUrl)
{
  new (Class="WTSS",serverUrl = serverUrl)
}

#*******************************************************
#SHOW
#*******************************************************
setMethod (
  
  # Name of the function
  f = "show", 
  
  # Method signature
  signature = "WTSS", 
  
  # Stylish print of the objects
  definition=function(object)
  {
    
    # initial message
    cat(paste("Object of Class WTSS\n\n"))
    
    # print serverUrl
    cat(paste("serverUrl: ",paste(object@serverUrl),  "\n"))
    
    # print listCoverages
    cat("listCoverages: ")
    cat(paste(object@listCoverages), " ")
    
    return(invisible())
  }
)
setGeneric(name="show", def=function(object){standardGeneric("show")})

#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object's server URL
#'
#' @param object A WTSS object
#' @docType methods
#' @aliases getServerUrl-generic
#' @export

setGeneric("getServerUrl",function(object){standardGeneric ("getServerUrl")})

#' @rdname getServerUrl
setMethod("getServerUrl","WTSS",
          function(object)
          {
            if(substr(object@serverUrl,nchar(object@serverUrl),nchar(object@serverUrl))!="/")
              return(paste(object@serverUrl,"/",sep=""))
            return(object@serverUrl)
          }
)

#' Sets the object's server URL
#'
#' @param object A WTSS object
#' @param aServerUrl A character representing the server URL.
#' @docType methods
#' @export
setGeneric("setServerUrl",function(object, aServerUrl){standardGeneric ("setServerUrl")})

#' @rdname  setServerUrl
setMethod("setServerUrl","WTSS",
          function(object, aServerUrl){
            object@serverUrl <- aServerUrl
          }
)

#' List coverages 
#'
#' @param object A WTSS object
#' @docType methods
#' @export
#' @examples
#' obj = WTSS("http://www.dpi.inpe.br/ts/wtss")
#' objlist = listCoverages(obj)
setGeneric("listCoverages",function(object){standardGeneric ("listCoverages")})

#' @rdname  listCoverages
setMethod("listCoverages","WTSS",
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
    #  cat('parseJSON\n')
      items <- .parseJSON(.sendRequest(request))
    #  cat('performed - parseJSON\n')
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
#' @param object A WTSS object
#' @param coverages A character vector of coverage names
#' @docType methods
#' @export
#' @examples
#' obj = WTSS("http://www.dpi.inpe.br/ts/wtss")
#' objdesc = describeCoverage(obj,listCoverages(obj)[1])
setGeneric("describeCoverage",function(object,coverages){standardGeneric("describeCoverage")})


#' @rdname  describeCoverage
setMethod("describeCoverage","WTSS",
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
  
  if(length(url) == 1 && nchar(url) > 1)
  {
    out <- lapply(coverages, function(cov)
          {
        
            # concat describe_coverage according to a name into the service URL 
            request <- paste(url,"describe_coverage?name=", cov, sep="")
            
            # avoid time out connection 
            while(class(items) == "try-error" & ce < 10) {
               items <- .parseJSON(.sendRequest(request))
               ce <- ce + 1
            }
            
            # if the server does not answer any item
            if (class(items) == "try-error")
              return(items)
            
            return(items)
        })
    
    names(out) <- coverages
    
    return(out)
  
  }
  
}

#' Get list of time series
#'
#' @description This function retrieves the time series for a list of coordinates.
#'
#' @param object Either a WTSS object or a server URL.
#' @param coverages Either a list of coverages and attributes such as retrieved by describe_coverage() or a character with the coverage name.
#' @param attributes A character vector of dataset names.
#' @param coordinates A list or data frame of longitude latitude coordinates in WGS84 coordinate system.
#' @param start A character with the start date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @param end A character with the end date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @docType methods
#' @export
#' @examples
#' obj = WTSS("http://www.dpi.inpe.br/tws/wtss")
#' objlist = listCoverages(obj)
#' objdesc = describeCoverage(obj,objlist[2])
#' coordinates = list(c(-45,-12),  c(-54,-11))
#' attributes = objdesc[[1]]$attributes$name[1]
#' tsList = listTimeSeries(obj, names(objdesc), attributes, coordinates, "2014-01-01", "2015-01-01")
setGeneric("listTimeSeries",function(object,coverages,attributes,coordinates,start,end){standardGeneric("listTimeSeries")})

#' @rdname  listTimeSeries
setMethod("listTimeSeries","WTSS",
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
#' @param object Either a WTSS object or a server URL
#' @param coverages Either a list of coverages and attributes such as retrieved by describe_coverage() or a character with the coverage name.
#' @param attributes A character vector of dataset names.
#' @param longitude A longitude in WGS84 coordinate system.
#' @param latitude A latitude in WGS84 coordinate system.
#' @param start A character with the start date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @param end A character with the end date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @docType methods
#' @export
#' @examples
#' obj = WTSS("http://www.dpi.inpe.br/tws/wtss")
#' objlist = listCoverages(obj)
#' objdesc = describeCoverage(obj,objlist[2])
#' attributes = objdesc[[1]]$attributes$name[1]
#' ts = timeSeries(obj, names(objdesc), attributes, -45,-12,"2000-02-18","2004-01-01")
setGeneric("timeSeries",function(object,coverages,attributes,longitude,latitude,start,end){standardGeneric("timeSeries")})

#' @rdname  timeSeries
setMethod("timeSeries","WTSS",
          function(object,coverages,attributes,longitude,latitude,start,end)
          {
          
            .timeSeries(object,coverages,attributes,longitude,latitude,start,end)
            
          }
)

.timeSeries <- function(object,coverages,attributes,longitude,latitude,start,end)
{
  
  if(missing(object))
    stop("Missing either a WTSS object or a server URL.")
  
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  url <- object
  
  if(class(object)=="WTSS")
    url <- getServerUrl(object)
  
  if(length(url) == 1 && nchar(url) > 1)
  {
    
    if(is.list(coverages))
    {
      
      out <- lapply(names(coverages), function(cov)
             {
                #attributes <- coverages[[cov]]
                
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
  else # if weekly or daily date
      if(format == "%Y-%m-%d")
        timeline = as.Date(timeline, format)
  
  return(list(center_coordinate = data.frame(longitude=items$result$center_coordinate$longitude, latitude=items$result$center_coordinate$latitude), 
              attributes = zoo(attributes.processed, timeline)))
  
}

#' ts to STFDF
#'
#' @description This function coerce data from the time series into STFDF data
#' 
#' @param timeseries Time series.
#' @docType methods
#' @export
#' @examples
#' chronos = WTSS("http://www.dpi.inpe.br/tws/wtss")
#' coverages  = listCoverages(chronos)
#' cv = describeCoverage(chronos, coverages[2])
#' attributes = cv[[1]]$attributes$name[1] 
#' ts = timeSeries(chronos, names(cv), attributes, -55,-13,"2000-02-18","2001-01-01")
#' stfdf <- as.STFDF(ts)
setGeneric("as.STFDF",function(timeseries){standardGeneric("as.STFDF")})
 
#' @rdname as.STFDF
setMethod("as.STFDF","list",
          function(timeseries)
          {
            .as.STFDF(timeseries)
          }
)

.as.STFDF <- function(timeseries) 
{
  
  if (length(index(timeseries[[1]]$attributes)) <= 1)
    cat("It is not possible to coerce from data into STFDF.")
  else
  {
     # STFDF return
     return(STFDF(SpatialPoints(timeseries[[1]]$center_coordinate), index(timeseries[[1]]$attributes), data.frame(coredata(timeseries[[1]]$attributes))))
   }
  
  return (NULL)
 
}

#' get Time Series by polygon
#'
#' @description This function coerce data from the time series into STFDF data
#' 
#' @param object Either a WTSS object or a server URL
#' @param cv Either a list of coverages and attributes such as retrieved by describe_coverage() or a character with the coverage name.
#' @param attributes A character vector of dataset names.
#' @param polygon A polygon space.
#' @param start A character with the start date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @param end A character with the end date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @docType methods
#' @export
setGeneric("polygonTimeSeries",function(object, cv, attributes, polygon, start, end){standardGeneric("polygonTimeSeries")})

#' @rdname polygonTimeSeries
setMethod("polygonTimeSeries","WTSS",
          function(object, cv, attributes, polygon, start, end) 
          {
            .polygonTimeSeries(object, cv, attributes, polygon, start, end) 
          }
)

.polygonTimeSeries <- function(object, cv, attributes, polygon, start, end) 
{
  
  x <- c(cv[[1]]$geo_extent$spatial$extent$xmin, cv[[1]]$geo_extent$spatial$extent$xmax)
  y <- c(cv[[1]]$geo_extent$spatial$extent$ymin, cv[[1]]$geo_extent$spatial$extent$ymax)
  spatial_extent <- bbox(SpatialPoints(cbind(x,y)))
  
  spatial_resolution <- c(x = cv[[1]]$geo_extent$spatial$resolution$x, y = cv[[1]]$geo_extent$spatial$resolution$y)
  
  minimum_bb = bbox(polygon)
  
  intersect_spatial <- bbox(intersect(minimum_bb, spatial_extent))
  
  # left inferior boundary
  lat_li = intersect_spatial[1]
  lon_li = intersect_spatial[2]
  lat_rs = intersect_spatial[3]
  lon_rs = intersect_spatial[4]
  
  # center coordinates resolution
  resolution = 0.05
  
  # initializing variables
  list_coordinates <- list()
  list_time_series <- list()
  n_elem = 1
  
  for (i in seq(from=lat_li, to=lat_rs, by=resolution)) {
    for (j in seq(from=lon_li, to=lon_rs, by=resolution)) {
      ts = timeSeries(object, names(cv), attributes=attributes, latitude=i, longitude=j, start=start, end=end)
      if (Position(function(x) identical(x, c(ts$MOD13Q1$center_coordinate$longitude,ts$MOD13Q1$center_coordinate$latitude)), list_coordinates, nomatch = 0) > 0) {
        cat("longitude = ", ts[[1]]$center_coordinate$longitude, "latitude = ", ts$MOD13Q1$center_coordinate$latitude, "\n")
        next
      }
      list_coordinates[[n_elem]] <- c(ts$MOD13Q1$center_coordinate$longitude,ts$MOD13Q1$center_coordinate$latitude)
      list_time_series[[n_elem]] <- list(center_coordinates = c(ts$MOD13Q1$center_coordinate$longitude,ts$MOD13Q1$center_coordinate$latitude), values = ts$MOD13Q1$attributes$ndvi)
      n_elem=n_elem+1
    }
  }
  
  return (list_time_series)
  
}

.sendRequest <- function(request)
{
  
  # check if URL exists and perform the request
  tryCatch(response <- getURL(request), 
           error = function(e) {
              e$message <- paste("HTTP request failed. The URL server may be incorrect or the time series service does not exist."); 
              stop(e);
           })
  
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
    json_response <- toJSON("ERROR: Request to the web time series service failed!")
    class(json_response) <- "try-error"
  }
  
  return(json_response)
}