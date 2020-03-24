context("Client version")

test_that("can get client version", {
  skip_on_cran()
  obj <- get.motu.client.version(CMEMS.config())
  expect_error(obj,NA)
})
