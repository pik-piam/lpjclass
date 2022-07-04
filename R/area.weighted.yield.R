#' area.weighted.yield
#' 
#' Function calculates cell-and crop-specific area-weighted mean yield from
#' rainfed and irrigated yield from a lpj object.
#' 
#' Default for nbands is 32 for LPJmL runs with 16 crops (incl. sugarcane and
#' bioenergy), default for year is 2000
#' 
#' @usage area.weighted.yield(data, nbands=32, year=2000, landusefile,
#' gridfile)
#' @param data lpj object
#' @param nbands integer. Number of bands in yield and landuse input file from
#' LPJmL run.
#' @param year integer. Landuse input file will be read only for this year and
#' used as weight.
#' @param landusefile character. Name of landuse input file used in LPJmL run
#' incl. name of folder
#' @param gridfile character. Name of grid output file from LPJmL run incl.
#' name of folder
#' @return The function returns a list, with 2 lpj objects: "data" containing
#' the yield and "croparea" containing the crop area in ha
#' @author Katharina Waha
#' @export
#' @seealso \code{\link{readLPJ}}, \code{\link{new.lpj}}
#' @examples
#'  \dontrun{mean.yield<-area.weighted.yield(yield,landusefile="D:/LPJ/input/cft1700_2005_16cfts_SR.bin",
#' 					     gridfile="D:/LPJ/run1/grid.bin")}
#'  
area.weighted.yield <- function (data,nbands=32,year=2000,landusefile,gridfile)
{
      if (!is.lpj(data)) stop ("can only read lpj object, please use as.lpj for transformation from array to lpj object")
	#settings
	refyear<-1700
      format<-2
      ncells.input<-67420
      ncells.output<-59199
      ncft<-nbands/2
    
      #read coordinates of output grid
	zz <- file(gridfile,"rb")
	x <- readBin(zz, integer(), n=format*ncells.output, size=format) / 100
	cellarea <- (111e3*0.5)*(111e3*0.5)*cos(x[c(1:ncells.output)*format]/180*pi)
	close(zz)
	rm(x,zz)

    	#read landuse input file
	yy<-year-refyear
	zi <- file(landusefile,"rb")
	seek(zi,where=38+(yy*(nbands*ncells.input)*format),origin="start") 
	temp<-readBin(zi,integer(),n=nbands*ncells.input,size=format)
	close(zi)
	temp<-array(temp,dim=c(nbands,ncells.input))/1000
	
	lpjclassdata <- NULL
	  data("lpjclassdata", envir = environment(), package="lpjclass")
	
	landuse<-temp[,lpjclassdata$grid_67420_59199!=0]

      crop.area<-new.lpj(dimnames(data)[[1]],paste("y",year,sep=""),dimnames(data)[[3]],dimnames(data)[[4]]) #in ha
	
      for (cft in 1:ncft){
  	 crop.area[,"y2000",cft,"rainfed"]<-(cellarea/10000)*landuse[cft,] 
       crop.area[,"y2000",cft,"irrigated"]<-(cellarea/10000)*landuse[cft+ncft,] 
      }

      data.mixed<-new.lpj(dimnames(data)[[1]],dimnames(data)[[2]],dimnames(data)[[3]],"mixed") 

      for (yy in 1:length(dimnames(data)[[2]]))
	   for (cft in 1:length(dimnames(data)[[3]]))
		data.mixed[,yy,cft,"mixed"]<-(data[,yy,cft,"rainfed"]*crop.area[,"y2000",cft,"rainfed"]+
			data[,yy,cft,"irrigated"]*crop.area[,"y2000",cft,"irrigated"])/rowSums(crop.area[,"y2000",cft,])
      return(list(data=data.mixed,croparea=crop.area))    
}
