context("Client version")

test_that("can get client version", {
  obj <- get.motu.client.version(CMEMS.config())
  expect_error(obj,NA)
})
