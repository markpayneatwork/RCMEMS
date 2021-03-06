% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classes.r
\name{show,CMEMS.config-method}
\alias{show,CMEMS.config-method}
\title{Show a CMEMS.config object}
\usage{
\S4method{show}{CMEMS.config}(object)
}
\arguments{
\item{object}{CMEMS.config object to show.}
}
\description{
Provides a brief overview of the contents of a CMEMS.config object
}
\examples{
cfg <- CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
                    service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
                    product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
                    variable = c("analysed_sst","analysis_error"))
show(cfg)
#Or mor simply
cfg
}
