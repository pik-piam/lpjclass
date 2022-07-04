#' Class "lpj" ~~~
#' 
#' The LPJ class is a data format for cellular LPJ data with a close
#' relationship to the array data format. \code{is.lpj} tests if \code{x} is an
#' LPJ-object, \code{as.lpj} transforms \code{x} to an LPJ-object (if
#' possible).
#' 
#' 
#' @name lpj-class
#' @aliases lpj-class as.lpj is.lpj [,lpj-method [,lpj,ANY,ANY-method
#' [<-,lpj-method [<-,lpj,ANY,ANY-method
#' @docType class
#' @param x An object that should be either tested or transformed as/to an
#' LPJ-object.
# @section Slots: \describe{ \item{list(".Data")}{Object of class \code{"array"} ~~ } \item{:}{Object of class \code{"array"} ~~ } }
#' @author Jan Philipp Dietrich, Susanne Rolinski, Katharina Waha, Benjamin
#' Bodirsky
#' @exportClass lpj
#' @exportMethod round [ [<-
#' @seealso \code{\link{readLPJ}}, \code{\link{read.LPJ_input}}
#' @keywords classes
#' @examples
#' 
#' showClass("lpj")
#' 
# @usage is.lpj(x) as.lpj(x)
setClass("lpj",contains="array",prototype=array(0,c(0,0,0,0)))

setMethod("[",
    signature(x = "lpj"),
    function (x, i, j, k, l, drop=FALSE)
    {
        if(!missing(i)) if(is.character(i)) {
          i <- grep(paste(paste(i,".",sep=""),collapse="|"),dimnames(x)[[1]])
        }       
        x@.Data <- x@.Data[i,j,k,l,drop=FALSE]
        return(x)
    }
)

setMethod("[<-",
    signature(x = "lpj"),
    function (x, i, j, k,l, value)
    {
        if(!missing(i)) if(is.character(i)) {
          i <- grep(paste(paste(i,".",sep=""),collapse="|"),dimnames(x)[[1]])
        }       
      x@.Data[i,j,k,l] <- value
      return(x)
    }
)

setMethod("round",
    signature(x = "lpj"),
    function (x, digits=0) 
    {      
        x@.Data <- round(x@.Data,digits=digits)
        return(x)
    }
)
