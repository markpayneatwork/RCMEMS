context("Download data")

#Use OSTIA reprocessed daily as a test product
#Using a single configuration makes it easier to maintain....
test.cfg <- 
  CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
               service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
               product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
               variable = "analysed_sst")

#Setup - assuming  motuclient module installed
tovar.test <- update(test.cfg,
                     variable=c("analysed_sst","sea_ice_fraction"))

test_that("can download data", {
  skip_if_offline()
  fname <- tempfile(fileext = ".nc")
  obj <- CMEMS.download(test.cfg,
                 ROI = extent(8,13,55,59),
                 date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,2)),
                 fname,
                 debug=FALSE,
                 quiet=TRUE)
  expect_true(file.exists(fname))
})

test_that("can download multiple variables", {
  skip_if_offline()
  fname <- tempfile(fileext = ".nc")
  obj <- CMEMS.download(tovar.test,
                        ROI = extent(8,13,55,59),
                        date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,2)),
                        fname,
                        debug=FALSE,
                        quiet=TRUE)
  expect_true(file.exists(fname))
})


