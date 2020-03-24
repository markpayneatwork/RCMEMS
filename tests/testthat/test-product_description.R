context("Retrieve product information")

#Setup - assuming  motuclient module installed
OSTIA.cfg <- 
  CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
               service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
               product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST")

PSY4.cfg <- 
  CMEMS.config(motu="http://nrt.cmems-du.eu/motu-web/Motu",
               service.id = "GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS",
               product.id = "global-analysis-forecast-phy-001-024-monthly")
 

test_that("can download product description - times", {
  obj <- product.description(OSTIA.cfg,"times",quiet=TRUE)
  expect_error(obj,NA)
  expect_equal(obj,"1985-01-01T12:00:00Z/2007-12-31T12:00:00Z/P1D")
})

test_that("can download product description  - depths", {
  obj <- product.description(PSY4.cfg,"depths",quiet=TRUE)
  expect_error(obj,NA)
})
