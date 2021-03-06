#' readLPJ
#' 
#' Reads a file which contains LPJ/LPJmL output and converts to a LPJ-object
#' 
#' This function reads in LPJ/LPJmL output files. So far, tested for yield,
#' monthly runoff, monthly temperature, monthly precipiation.  Note, that
#' number of years in file will be calculated automatically if years=NULL
#' 
#' @usage readLPJ(file_name, wyears, syear=1901, averaging_range = NULL,
#' file_folder = "", file_type = NULL, bands = 41, monthly = FALSE, bytes = 4,
#' years = NULL, ncells = 59199,soilcells=FALSE, headlines = 0, datatype =
#' numeric(), gridfile = "", flexbands=FALSE)
#' @param file_name a character string naming a file with extension but without
#' folder
#' @param wyears integer. a vector containing the years of interest
#' @param syear integer. The first year
#' @param averaging_range integer.
#' @param file_folder a character string naming the folder where the file is
#' stored
#' @param file_type a character string, usually "bin" for binary files
#' @param bands integer. The number of bands (e.g. crops, months). Will be 12
#' if monthly==TRUE
#' @param monthly logical.
#' @param bytes integer. The size of data type
#' @param years integer. The number of years in file
#' @param ncells integer.
#' @param soilcells Bool. If 67420 cells are present in the file, should only
#' the 59199 for MAgPIE be returned?
#' @param headlines integer. The size of header, for output files = 0
#' @param datatype a R data type
#' @param gridfile a character string, currently not used
#' @param flexbands reads in cfts for any number of bands, assumes only cfts, must be named later
#' @return \item{x}{LPJ-object}
#' @author Susanne Roliniski, Benjamin Bodirsky
#' @export
#' @importFrom utils tail data
#' 
#' @seealso \code{\link{read.LPJ_input}}
#' @examples
#' 
#'   ##Example for the LPJmL output file pft_harvest.pft.bin containing crop yields in gC/m2
#'   \dontrun{yield<-readLPJ(file_name="pft_harvest.pft.bin",wyears=c(1998:2002),
#'   file_folder="D:/LPJ/Trunk_2010_03_25/output/simulated_sdate/",
#' 	bands=26,gridfile="D:/LPJ/Trunk_2010_03_25/output/simulated_sdate/grid.bin")} 
#' 
# lpj_in_out provides functions to read LPJmL files in an appropiate 4D-array and 
# to write it back to file consistent with magpie_in_out
# Version 1.11 - Susanne Rolinski, Jan Philipp Dietrich, Benjamin Bodirsky
# 1.01: added automatic calculation of number of years (jpd)
# 1.02: included combined averaging/multi-year-output procedure in readLPJ (jpd)
# 1.03: included procedure for monthly lpj data (jpd)
# 1.04: switched begr and betr in band_names (jpd)
# 1.05: improved readLPJ which can read now additional lpj outputs (bb)
# 1.06: bugfix: "land" was defined after its first use! (jpd)
# 1.07: read.LPJ_input added (bb)
# 1.08: read.LPJ_input bugfix (bb)
# 1.09: bugfix in readLPJ (year + 1 instead of year was read) (jpd)
# 1.10: added error for invalid choice of averaging range or wyears (jpd)
# 1.11: corrected typo in lpj names ("groudnut") (jpd)
# 1.12: added case bands=35 (pfts + cfts 1-13) (jpd)
#1.13: add flexbands switch 

readLPJ <- function(file_name,             # Filename with or without extention and folder
                    wyears,                # year or years of output
                    syear=1901,            # first year of simulation (standard=1901)
                    averaging_range=NULL,  # number of years that should be used for averaging
                    file_folder="",        # folder of lpj file
                    file_type=NULL,        # extention of lpj file
                    bands=41,              # number of crop bands in lpj output, 16 for cfts, 9 for pfts, 41 for pfts, rainfed and irrigated cfts
                    monthly=FALSE,         # switch for monthly data
                    bytes=4,               # size of data in binary lpj file
                    years=NULL,            # number of simulated years
                    ncells=59199,          # number of grid cells in lpj file
                    soilcells=FALSE,       # should the cells with no soil information be discarded (only for 67420 cells)
                    headlines=0,           # number of lines in the header of lpj file
                    datatype=numeric(),  # respective gridfile in same folder
                    gridfile="",
                    flexbands=FALSE     ) {    # option to read any number of cft bands 
  #require(ludata)
 # data("ludata", envir = environment(), package="ludata")
#  lud <- ludata
 # lud <- ludata
  #Skript compatibility for previous versions:
  if (monthly==TRUE) {bands<-12}
  #if file-type is not mentioned file-ending is used as file-type
  if (is.null(file_type)) {
    file_type <- tail(strsplit(file_name,'\\.')[[1]],1)
  }
    file_name <- paste(file_folder,file_name,sep="")


    if (length(Sys.glob(file_name))==0) {
      stop(paste("file",file_name,"does not exist"))
    }

    #expand wildcards
  file_name_unexpanded <- file_name
  file_name <- Sys.glob(file_name)
  band_names_cfts <- c("tece","rice","maize","trce","pulses","tero","trro","sunflower",
  "soybean","groundnut","rapeseed","sugarcane","others","mgrass","begr","betr")
  band_names_pfts <-c("TROPICAL_BROADLEAVED_EVERGREEN_TREE","TROPICAL_BROADLEAVED_RAINGREEN_TREE","TEMPERATE_NEEDLELEAVED_EVERGREEN_TREE",
                      "TEMPERATE_BROADLEAVED_EVERGREEN_TREE","TEMPERATE_BROADLEAVED_SUMMERGREEN_TREE","BOREAL_NEEDLELEAVED_EVERGREEN_TREE",
                      "BOREAL_BROADLEAVED_SUMMERGREEN_TREE","C3_PERENNIAL_GRASS","C4_PERENNIAL_GRASS")
  month_names <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  pfts<-9
  cfts<-16
  month<-12

  
  # naming the array
  lpjclassdata <- NULL
  data("lpjclassdata", envir = environment(), package="lpjclass")
  land <- lpjclassdata$cellbelongings[,c("LPJ.Index","country.code")]
  land <- land[order(land$LPJ.Index),]
  land$countryname <- lpjclassdata$countryregions$country.name[land$country.code+1]


  if (length(file_name)>1) {
    file_name <- file_name[1]
    warning(paste("file name",file_name_unexpanded,"is ambiguous, only first alternative is used!"))
  }


    if (file.exists(file_name)) {
    if (file_type=="bin") {
      ss <- file.info(file_name)$size
      ssz <- (ss-headlines)/bands/bytes
      # check for number of bands
      if (round(ssz) != ssz) {
        obands <- bands
        nofit <- TRUE
        while (nofit) {
          bands <- bands-1
          ssz <- (ss-headlines)/bands/bytes
          if (round(ssz) == ssz) nofit <- FALSE
        }
        warning(paste("number of bands (",obands,") changed to",bands))
      }

      # check for number of cells
      ssz <- (ss-headlines)/bands/bytes
      if (round(ssz/ncells) != ssz/ncells){
        if(round(ssz/59199)==ssz/59199){
          warning("ncells changed from ",ncells, "to 59199")
          ncells<-59199
        } else if(round(ssz/67420)==ssz/67420){
          warning("ncells changed from ",ncells, "to 67420")
          ncells<-67420
        } else{
          stop(paste("number of cells in file are not",ncells))
        }
      }
      
      if (length(file_name)>1) {
        file_name <- file_name[1]
        warning(paste("file name",file_name_unexpanded,"is ambiguous, only first alternative is used!"))
      }

      if(is.null(years)){
        years <- (ss-headlines)/bands/ncells/bytes
        if(years!=round(years)) stop(paste("inconsistent data set, calculation of number of years delivered an non-integer result of",years))
      } else {
        # check for number of years
        ssz <- (ss-headlines)/bands/bytes/years
        if (round(ssz) != ssz) stop(paste("number of years in file are not",years))
      }
      
     outputdimnames <- list() 
     if(ncells==59199){
       outputdimnames[[1]] <- paste(land$countryname,1:ncells,sep=".")
     } else{
       outputdimnames[[1]]<-1:ncells
     }
     outputdimnames[[2]] <- paste("y",wyears,sep="")

     if(bands==month) {
          subbands <- 1
          outputdimnames[[3]] <- month_names
          outputdimnames[[4]] <- list("data")
      } else if(bands==pfts+cfts+cfts) {
          subbands <- 1
          outputdimnames[[3]] <- c(band_names_pfts, band_names_cfts)
          outputdimnames[[4]] <- c("rainfed","irrigated")
      } else if(bands==pfts) {
          subbands <- 1
          outputdimnames[[3]] <- band_names_pfts
          outputdimnames[[4]] <- list("data")
      } else if(bands==1) {
          subbands<-1
          outputdimnames[[3]] <- list("x")
          outputdimnames[[4]] <- list("data")
      } else if(bands==5) {
          subbands<-1
          outputdimnames[[3]] <- paste0("layer",1:5)
          outputdimnames[[4]] <- list("data")
      } else if (bands==2*cfts) {
         subbands<-2
         outputdimnames[[3]] <- band_names_cfts
         outputdimnames[[4]] <- c("rainfed","irrigated")
      } else if (bands==2*(cfts-1)) {
         outputdimnames[[3]] <- band_names_cfts[-12]
         outputdimnames[[4]] <- c("rainfed","irrigated")
      } else if (bands==2*(cfts-2)) {
         subbands<-2
         outputdimnames[[3]] <- band_names_cfts[-c(15,16)]
         outputdimnames[[4]] <- c("rainfed","irrigated")
      } else if (bands==2*(cfts-3)) {
         subbands<-2
         outputdimnames[[3]] <- band_names_cfts[-c(12,15,16)]
         outputdimnames[[4]] <- c("rainfed","irrigated")
      } else if (bands == 2 * (cfts - 4)) {
                subbands <- 2
                outputdimnames[[3]] <- band_names_cfts[1:12]
                outputdimnames[[4]] <- c("rainfed", "irrigated")
      } else if(bands==pfts+2*13) {
          cfts <- 13
          band_names_cfts <- band_names_cfts[1:13]
          subbands <- 1
          outputdimnames[[3]] <- c(band_names_pfts, band_names_cfts)
          outputdimnames[[4]] <- c("rainfed","irrigated")
      } else {
        if(flexbands){
          warning("non-standard number of bands: assume reading only cfts, must still be named!")
          outputdimnames[[3]] <- c(paste0("cft_",seq(1,bands)))
          outputdimnames[[4]] <- c("rainfed","irrigated")
        } else {
          stop("Unknown number of bands")
        }
      }

      #Prepare averaging
      if(is.null(averaging_range)) averaging_range <- 1
      if(averaging_range<1) {
        warning(paste("Invalid choice of averaging_range. Value",averaging_range,"is not allowed! Value is set to 1 instead!"))
        averaging_range <- 1
      }
      #in the case of an even number of years, that should be used for averaging, the average is not symmetric to the corresponding year
      #in this case one year more is taken in the past then in the future of the correpsonding year
      averaging_steps <- -floor(averaging_range/2)+(0:(averaging_range-1))


      # reading the file
      zz <- file(file_name,"rb")

      output <- array(data=0,dim=c(ncells,length(wyears),bands))

      for(wyear in wyears) {
        bet <- rep(0,ncells*bands)
        for(avg in averaging_steps) {
          if(wyear+avg-syear<0) stop("Invalid choice of years or averaging range. Years before the start year of the data set are required!")
          seek(zz,where=headlines+(wyear+avg-syear)*bands*ncells*bytes,origin="start")
          bet <- bet + readBin(zz,datatype,n=ncells*bands,size=bytes)
        }
        output[,match(wyear,wyears),] <- matrix(bet/length(averaging_steps),nrow=ncells)[,(1:bands)]
      }
      rm(bet)
      close(zz)
      # adding an irrigated dimension to pfts in files with 41 bands
      if(bands==pfts+cfts+cfts) {
         output <- array(output[,,c(1:(pfts+cfts),1:pfts,(pfts+cfts+1):(pfts+cfts*2))],dim=sapply(outputdimnames,length),dimnames=outputdimnames)
         output[,,1:pfts,2] <- NA
      }  else {
         output <- array(output,dim=sapply(outputdimnames,length),dimnames=outputdimnames)
      }
    }
    } else {
      warning(paste("File",file_name,"does not exist"))
      return(as.lpj(NULL))
    }
  if(ncells==67420 && soilcells==TRUE){
    lpjclassdata <- NULL
      data("lpjclassdata", envir = environment(), package="lpjclass")
   # data("lpjclassdata",package = "lpjclass")
    output<-output[which(lpjclassdata$grid_67420_59199==1),,,,drop=FALSE]
    dimnames(output)[[1]]<-paste(land$countryname,1:59199,sep=".")
  }
  return(new("lpj",output))
}
