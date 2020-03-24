context("Donwnload data")

#Setup - assuming  motuclient module installed
OSTIA.cfg <- 
  CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
               service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
               product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
               variable = "analysed_sst")

tovar.test <- 
  CMEMS.config(motu="http://nrtcmems.mercator-ocean.fr/motu-web/Motu",
               service.id="GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS",
               product.id="global-analysis-forecast-phy-001-024-monthly",
               variable=c("so","bottomT"))

test_that("can download data", {
  fname <- tempfile(fileext = ".nc")
  obj <- CMEMS.download(OSTIA.cfg,
                 ROI = extent(8,13,55,59),
                 date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,10)),
                 fname,
                 debug=FALSE,
                 quiet=TRUE)
  expect_true(file.exists(fname))
})

test_that("can download multiple variables", {
  fname <- tempfile(fileext = ".nc")
  obj <- CMEMS.download(tovar.test,
                        ROI = extent(8,13,55,59),
                        date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,10)),
                        fname,
                        debug=FALSE,
                        quiet=TRUE)
  expect_true(file.exists(fname))
})


