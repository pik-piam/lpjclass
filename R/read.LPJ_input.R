#' read.LPJ_input
#'
#' Reads a LPJmL input file and converts it to a LPJ-object
#'
#' This function reads in LPJ-input files, using its header information. So
#' far, tested for landuse input. `rule4binary` combined with `namesum = T` allow for the rule based counting of events. 
#'
#' @param file_name Filename with extension and folder
#' @param out_years years to be red out in the form of a vector of year
#' strings, e.g. c(y1995,y2005)
#' @param namesum if true, all bands/colums of the dataset are added. Sometimes
#' useful to handle large datasets. Overwrites four_d
#' @param four_d if true, it is assumed that data exists for both rainfed and
#' irrigated crops.
#' @param ncells number of cells
#' @param swap Depends on the binary format of the data
#' @param rule4binary Rule based transformation of the outputs into binary values
#' @return \item{x}{LPJ-object}
#' @author Benjamin Bodirsky, Susanne Roliniski, Marcos Alves
#' @export
#' @importFrom magclass isYear
#' @seealso \code{\link{readLPJ}}
read.LPJ_input <- function(file_name,               # Filename with or without extention and folder
                           out_years = c("y1995", "y2005"),                 # first year of simulation (standard=1901)
                           namesum = FALSE,
                           four_d = FALSE,
                           ncells = 59199,
                           swap = "little",
                           rule4binary = NULL
) {
  # require(ludata)
  # require(magclass)
  lpjclassdata <- NULL
  data("lpjclassdata", envir = environment(), package = "lpjclass")
  band_names_cfts <- c("tece", "rice", "maize", "trce", "pulses", "tero", "trro", "sunflower",
                       "soybean", "groundnut", "rapeseed", "sugarcane", "others", "mgrass", "begr", "betr")
  if (all(isYear(out_years) == TRUE) == FALSE) {
    stop("out_years has no appropriate year format")
  }
  out_nyears <- length(out_years)
  if (out_nyears == 1) {
    out_years <- list(out_years)
  }
  grid_67420_59199 <- lpjclassdata$grid_67420_59199
  in_headbytes <- 43
  
  
  # grid_67420_59199<- as.integer(readBin("soil.bin",what=raw(),size=1,n=67420))
  # grid_67420_59199[which(grid_67420_59199>=1)]<-1
  # landusedata$grid_67420_59199<-grid_67420_59199
  
  if (ncells == 67420) {
    out_ncells <- length(grid_67420_59199)
  } else if (ncells == 59199) {
    out_ncells <- length(grid_67420_59199[which(grid_67420_59199 != 0)])
  } else {
    stop("Wrong number of cells provided (use 67420 or 59199).")
  }
  
  
  filedata <- file(description = file_name, open = "rb", blocking = TRUE, encoding = getOption("encoding"))
  seek(filedata, where = 7, origin = "start")
  in_header   <- as.numeric(readBin(filedata, what = integer(), size = 4, n = 7, endian = .Platform$endian))
  in_syear    <- in_header[3]
  in_nyears   <- in_header[4]
  in_ncells   <- in_header[6]
  in_ncolumns <- in_header[7]
  seek(filedata, where = 7 + 7 * 4, origin = "start")
  in_header   <- readBin(filedata, what = numeric(), size = 4, n = 2, endian = .Platform$endian)
  in_cellsize <- in_header[1]
  in_scalar   <- in_header[2]
  
  in_years    <- paste("y", in_syear + (1:in_nyears) - 1, sep = "")
  in_nbytes   <- 2
  
  in_file_size <- file.info(file_name)$size
  if (in_file_size != in_headbytes + in_nyears * in_ncells * in_ncolumns * as.double(in_nbytes)) {
    stop("file size does not fit header")
  }
  
  read_year_set <- function(year) {
    if ((year %in% in_years) == FALSE) {
      stop("out_year is not in the dataset")
    }
    year_position <- which(in_years == year)
    startread <- in_headbytes + (year_position - 1) * in_ncells * in_ncolumns * in_nbytes
    seek(filedata, where = startread, origin = "start")
    yeardataset_bin <- readBin(filedata, what = integer(), size = in_nbytes, n = in_ncolumns * in_ncells, endian = swap)
    if (!is.null(rule4binary)) {
      condition <- paste("yeardataset_bin", rule4binary)
      x_check <- eval(parse(text = condition))
      yeardataset_bin[x_check] <- 1
      yeardataset_bin[!x_check] <- 0
    }
    yeardataset <- array(yeardataset_bin, dim = c(in_ncolumns, in_ncells))
    if (ncells == 59199) yeardataset <- yeardataset[, which(grid_67420_59199 != 0)]
    return(t(yeardataset))
  }
  
  if (namesum) {
    out_dataset <- array(NA, dim = c(out_ncells, out_nyears, 1, 1))
    dimnames(out_dataset)[[2]] <- out_years
    dimnames(out_dataset)[[3]] <- c("sum")
    dimnames(out_dataset)[[4]] <- c("sum")
    for (year in out_years) {
      out_dataset[, year, 1, 1] <- rowSums(read_year_set(year = year))
    }
  } else if (four_d) {
    out_dataset <-  array(NA, dim = c(out_ncells, out_nyears, in_ncolumns / 2, 2))
    dimnames(out_dataset)[[2]] <- out_years
    dimnames(out_dataset)[[3]] <- band_names_cfts[1:(in_ncolumns / 2)]
    dimnames(out_dataset)[[4]] <- c("rainfed", "irrigated")
    for (year in out_years) {
      intermediate <- read_year_set(year = year)
      out_dataset[, year, , "rainfed"] <- intermediate[, 1:(in_ncolumns / 2)]
      out_dataset[, year, , "irrigated"] <- intermediate[, (in_ncolumns / 2) + 1:(in_ncolumns / 2)]
      rm(intermediate)
    }
  } else {
    out_dataset <-  array(NA, dim = c(out_ncells, out_nyears, in_ncolumns, 1))
    dimnames(out_dataset)[[2]] <- out_years
    dimnames(out_dataset)[[4]] <- c("no_name")
    for (year in out_years) {
      out_dataset[, year, , 1] <- read_year_set(year = year)
    }
  }
  close(filedata)
  # lud <-NULL
  #   data("ludata", envir = environment(), package="ludata")
  # lud <- ludata
  land <- lpjclassdata$cellbelongings[, c("LPJ.Index", "country.code")]
  land <- land[order(land$LPJ.Index), ]
  land$countryname <- lpjclassdata$countryregions$country.name[land$country.code + 1]
  dimnames(out_dataset)[[1]] <- paste(land$countryname, 1:out_ncells, sep = ".")
  
  out_dataset <- as.lpj(out_dataset)
  if(is.null(rule4binary)){
    out_dataset <- out_dataset * in_scalar
  }
  return(out_dataset)
}
