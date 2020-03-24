context("Parsing of script from CMEMS")

#Setup test scripts
module.test <- 'python -m motuclient --user <USERNAME> --pwd <PASSWORD> --motu http://nrtcmems.mercator-ocean.fr/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS --product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 --longitude-max 179.91667175293 --latitude-min -80 --latitude-max 90 --date-min "2007-01-16 12:00:00" --date-max "2018-01-16 12:00:00" --depth-min 186.1255 --depth-max 763.3333 --variable so -out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>'
script.test <- 'python home/motuclient.py --user <USERNAME> --pwd <PASSWORD> --motu http://nrtcmems.mercator-ocean.fr/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS --product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 --longitude-max 179.91667175293 --latitude-min -80 --latitude-max 90 --date-min "2007-01-16 12:00:00" --date-max "2018-01-16 12:00:00" --depth-min 186.1255 --depth-max 763.3333 --variable so -out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>'
tovar.test <- 'python -m motuclient --user <USERNAME> --pwd <PASSWORD> --motu http://nrtcmems.mercator-ocean.fr/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS --product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 --longitude-max 179.91667175293 --latitude-min -80 --latitude-max 90 --date-min "2007-01-16 12:00:00" --date-max "2018-01-16 12:00:00" --depth-min 186.1255 --depth-max 763.3333 --variable so --variable bottomT -out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>'

#Tests
test_that("script parses when using modules", {
  obj <- parse.CMEMS.script(module.test)
  expect_error(obj,NA)  #Expect it to work!
  expect_equal(obj@script,as.character(NA))   #Should be NA in the script slot
})


test_that("script parses with motuclient location specified", {
  obj <- parse.CMEMS.script(script.test)
  expect_error(obj,NA)  
  expect_equal(obj@script,"home/motuclient.py")  #Now something in the script slot
})

test_that("dates are parsed correctly",{
  obj <- parse.CMEMS.script(module.test)
  expect_equal(obj@date.min,"2007-01-16 12:00:00")
  expect_equal(obj@date.max,"2018-01-16 12:00:00")
})

test_that("multiple variables parsed correctly", {
  obj <- parse.CMEMS.script(tovar.test)
  expect_length(obj@variable,2)
})
