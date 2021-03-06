% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classes.r
\name{update}
\alias{update}
\alias{update,CMEMS.config-method}
\title{Update a CMEMS.config object}
\usage{
update(object, ...)

\S4method{update}{CMEMS.config}(object, ...)
}
\arguments{
\item{object}{The \code{CMEMS.config} object to serve as the basis for creating the new object}

\item{...}{Slots in the new object - see documentation of \code{\link{CMEMS.config}} for more details}
}
\description{
Updates a CMEMS.config object with new parameters
}
\examples{
#Setup a configuration object, using OSTIA as an example
cfg <- CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
                    service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
                    product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
                    variable = c("analysed_sst","analysis_error"))
cfg
#Now update the variables, for example
cfg2 <- update(cfg,variable="sea_ice_fraction")
cfg2
}
