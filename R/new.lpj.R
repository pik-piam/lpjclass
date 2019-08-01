#' new.lpj
#' 
#' Creates a new LPJ object
#' 
#' 
#' @usage new.lpj(cells_and_regions,years,names,irrigation)
#' @param cells_and_regions dimnames for cells in the format
#' "country.cellnumber"
#' @param years dimnames for years in the format "yXXXX"
#' @param names dimnames for names
#' @param irrigation dimnames for irrigation
#' @return an empty lpj object filled with NAs, with the given dimnames
#' @author Benjamin Bodirsky, Katharina Waha
#' @seealso \code{\linkS4class{lpj}},\code{\link{readLPJ}}
#' @export
#' @examples
#' 
#'  cells <- c("Russia.1","Russia.2")
#'  years <- "y1995"
#'  categories <- c("TeCe","TrRi")
#'  irrigation <- c("rainfed","irrigated")
#'  a <- new.lpj(cells_and_regions=cells,years=years,names=categories,irrigation=irrigation)
#'  a
#' 
new.lpj <- function (cells_and_regions, years, names, irrigation) 
{
    	object <- array(NA, dim = c(length(cells_and_regions), length(years), 
        length(names),length(irrigation)))
    	dimnames(object)[[1]] <- as.list(cells_and_regions)
    	dimnames(object)[[2]] <- as.list(years)
    	dimnames(object)[[3]] <- as.list(names)
    	dimnames(object)[[4]] <- as.list(irrigation)
    	object <- as.lpj(object)
    	return(object)
}