context("Download data")

#Setup - assuming  motuclient module installed
OSTIA.cfg <- 
  CMEMS.config(motu="http://my.cmems-du.eu/motu-web/Motu",
               service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
               product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST-MON",
               variable = "analysed_sst")

tovar.test <- update(OSTIA.cfg,
                     variable=c("analysed_sst","standard_deviation_sst"))

test_that("can download data", {
  skip_if_offline()
  fname <- tempfile(fileext = ".nc")
  obj <- CMEMS.download(OSTIA.cfg,
                 ROI = extent(8,13,55,59),
                 date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,31)),
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
                        date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,31)),
                        fname,
                        debug=FALSE,
                        quiet=TRUE)
  expect_true(file.exists(fname))
})


