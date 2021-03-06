% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CMEMS_interface.r
\name{CMEMS.download}
\alias{CMEMS.download}
\alias{CMEMS.download.advanced}
\title{Download from CMEMS}
\usage{
CMEMS.download(
  x,
  ROI = "missing",
  date.range = "missing",
  out.path = "missing",
  depth.range = "missing",
  ...
)

CMEMS.download.advanced(
  x,
  out.dir = NULL,
  out.name = NULL,
  date.min = NULL,
  date.max = NULL,
  latitude.min = NULL,
  latitude.max = NULL,
  longitude.min = NULL,
  longitude.max = NULL,
  depth.min = NULL,
  depth.max = NULL,
  quiet = FALSE,
  debug = FALSE
)
}
\arguments{
\item{x}{An object of class \code{\link{CMEMS.config}} containing the configuration parameters}

\item{ROI}{An vector of length 4 specifying the region of interest in the following order: 
minimum longitude, maximum longitude, minimum latitude, maximum latitude. Note that this 
structure mirrors that of the \code{raster::extent()} class and these can also be used
directly here instead.}

\item{date.range}{The maximum and minimum dates (vector of length two of class "Date")}

\item{out.path}{The output path, including the directory and filename in one string.}

\item{depth.range}{The maximum and minimum depths specified as a vector of length 2 (float in the interval [0 ; 2e31 ] -
does not accept 'Surface' as an argument)}

\item{...}{Arguments to be passed on further to the \code{\link{CMEMS.download.advanced}} function. Overwrites any arguments automatically generated
in \code{\link{CMEMS.download}} function.}

\item{out.dir}{The output dir (string)}

\item{out.name}{The output file name (string)}

\item{date.min}{The min date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS])}

\item{date.max}{The max date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS ])}

\item{latitude.min}{The min latitude (float in the interval [-90 ; 90 ])}

\item{latitude.max}{The max latitude (float in the interval [-90 ; 90 ])}

\item{longitude.min}{The min longitude (float in the interval [-180 ; 180 ])}

\item{longitude.max}{The max longitude (float in the interval [-180 ; 180 ])}

\item{depth.min}{The min depth (float in the interval [0 ; 2e31 ] or string 'Surface')}

\item{depth.max}{The max depth (float in the interval [0 ; 2e31 ] or string 'Surface')}

\item{quiet}{Logical value, indicating whether to supress output}

\item{debug}{Allows debugging of the motu client command - builds the command without running it (logical)}
}
\value{
If debug is TRUE, returns the full command to the motu client, ready to be run (via \code{system()}) or checked manually. If
debug is FALSE (the default), runs the command and returns the error code associated with the motuclient.
}
\description{
Provides R interfaces to the Python-based Motu client developed to provide access to 
CMEMS data. Two interfaces are provided - the \code{CMEMS.download} interface is a simple 
interface taking advantage of R classes, while the \code{\link{CMEMS.download.advanced}}
interface provides access to the full functionality of the motu client.
}
\details{
Arguments provided to  \code{\link{CMEMS.download}} and  \code{\link{CMEMS.download.advanced}} override
any arguments supplied in the \code{\link{CMEMS.config}} object, x.

If the  \code{\link{CMEMS.config}} object, x, is missing either the username or the
password, both are dropped from the call to motuclient - in this case, the client will 
use the local configuration file. See the README.md file supplied with motuclient for how
to set this up.
}
\examples{
\dontrun{
#Setup a configuration object, using OSTIA as an example
cfg <- CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
                    service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
                    product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
                    variable = c("analysed_sst","analysis_error"))
CMEMS.download(cfg,
               ROI = c(8,13,55,59),
               date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,10)),
               out.path="test.nc",
               debug=FALSE)
}
}
\seealso{
Details and documentation on the Motu client, including releases of the software 
to download, can be found on the associated GitHub site, \url{https://github.com/clstoulouse/motu-client-python}
}
