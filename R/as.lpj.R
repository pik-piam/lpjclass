#' @importFrom methods new 
#' @export

as.lpj <- function(x){
  if(is.array(x)) {
    if(length(grep("\\.",dimnames(x)[[1]]))==dim(x)[1] & length(dim(x))==4) {
      return(new("lpj",x))
    } else {
      stop("Input cannot be converted to lpj-format. Wrong structure!")
    }
  } else {
    if(is.null(x)) {
      return(new("lpj"))
    } else {
      stop("Input is not an array. Wrong structure!")
    }
  }  
}