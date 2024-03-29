% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readLPJ.R
\name{readLPJ}
\alias{readLPJ}
\title{readLPJ}
\usage{
readLPJ(
  file_name,
  wyears,
  syear = 1901,
  averaging_range = NULL,
  file_folder = "",
  file_type = NULL,
  bands = 41,
  monthly = FALSE,
  bytes = 4,
  years = NULL,
  ncells = 59199,
  soilcells = FALSE,
  headlines = 0,
  datatype = numeric(),
  gridfile = "",
  cellyear = FALSE,
  flexbands = FALSE
)
}
\arguments{
\item{file_name}{a character string naming a file with extension but without
folder}

\item{wyears}{integer. a vector containing the years of interest}

\item{syear}{integer. The first year}

\item{averaging_range}{integer.}

\item{file_folder}{a character string naming the folder where the file is
stored}

\item{file_type}{a character string, usually "bin" for binary files}

\item{bands}{integer. The number of bands (e.g. crops, months). Will be 12
if monthly==TRUE}

\item{monthly}{logical.}

\item{bytes}{integer. The size of data type}

\item{years}{integer. The number of years in file}

\item{ncells}{integer.}

\item{soilcells}{Bool. If 67420 cells are present in the file, should only
the 59199 for MAgPIE be returned?}

\item{headlines}{integer. The size of header, for output files = 0}

\item{datatype}{a R data type}

\item{gridfile}{a character string, currently not used}

\item{cellyear}{Switch for input data format. Cellyear is the new lpjml input format.
When reading in lpjml outputs set to false.}

\item{flexbands}{reads in cfts for any number of bands, assumes only cfts, must be named later}
}
\value{
\item{x}{LPJ-object}
}
\description{
Reads a file which contains LPJ/LPJmL output and converts to a LPJ-object
}
\details{
This function reads in LPJ/LPJmL output files. So far, tested for yield,
monthly runoff, monthly temperature, monthly precipiation.  Note, that
number of years in file will be calculated automatically if years=NULL
}
\examples{

## Example for the LPJmL output file pft_harvest.pft.bin containing crop yields in gC/m2
\dontrun{
yield <- readLPJ(file_name = "pft_harvest.pft.bin", wyears = c(1998:2002),
  file_folder = "D:/LPJ/Trunk_2010_03_25/output/simulated_sdate/",
        bands = 26, gridfile = "D:/LPJ/Trunk_2010_03_25/output/simulated_sdate/grid.bin")
}

}
\seealso{
\code{\link{read.LPJ_input}}
}
\author{
Susanne Rolinski, Benjamin Bodirsky
}
