context("Retrieve product description")

#Use OSTIA reprocessed daily as a test product
#Using a single configuration makes it easier to maintain....
test.cfg <- 
  CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
               service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
               product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
               variable = "analysed_sst")

test_that("can download product description - times", {
  skip_on_cran()
  obj <- product.description(test.cfg,"times",quiet=TRUE)
  expect_error(obj,NA)
  if(requireNamespace("xml2",quietly=TRUE)) {  #Only expect meaningful result if xml2 is installed
      expect_equal(obj,"1985-01-01T12:00:00Z/2007-12-31T12:00:00Z/P1D")
  }
})

test_that("can download product description  - depths", {
  skip_on_cran()
  obj <- product.description(test.cfg,"depths",quiet=TRUE)
  expect_error(obj,NA)
  if(requireNamespace("xml2",quietly=TRUE)) {  #Only expect meaningful result if xml2 is installed
    expect_true(is.na(obj))
  }
})
