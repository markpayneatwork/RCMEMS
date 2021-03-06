% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CMEMS_interface.r
\name{parse.CMEMS.script}
\alias{parse.CMEMS.script}
\title{Get configuration from CMEMS command-line script}
\usage{
parse.CMEMS.script(script, parse.user = FALSE)
}
\arguments{
\item{script}{A text string for a download script provided by the CMEMS website}

\item{parse.user}{A logical variable indicating whether the username and password should be parsed
or not. Typically, these are place holder values, so it's not really necessary to parse them.}
}
\description{
Extracts the available details from a download script provided by CMEMS and uses it
to create an CMEMS.config object
}
\details{
The CMEMS website has subsetting functionality that can be used as a template generate a download script for use with the motu
client (usually accessed by clicking on the "View Script" button when preparing to download). This function
saves the hard work of having to figure out the parameters for use with the \code{RCMEMS} package by parsing this command and
extracting the relevant information.
}
\examples{
#Using a script automatically generated by CMEMS
script <- 'python -m motuclient --motu http://my.cmems-du.eu/motu-web/Motu 
           --service-id SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS 
           --product-id METOFFICE-GLO-SST-L4-RAN-OBS-SST  --longitude-min -179.97500610351562 
           --longitude-max 179.97500610351562 --latitude-min -89.9749984741211 
           --latitude-max 89.9749984741211 --date-min "2007-12-26 12:00:00" 
           --date-max "2007-12-31 12:00:00"  --variable analysed_sst 
           --variable analysis_error --out-dir <OUTPUT_DIRECTORY> 
           --out-name <OUTPUT_FILENAME> --user <USERNAME> --pwd <PASSWORD> '
cfg <- parse.CMEMS.script(script)
cfg
}
