context("Client version")

test_that("can get client version", {
  skip_on_cran()
  obj <- get.motuclient.version()
  expect_error(obj,NA)
})

test_that("correct version number", {
  skip_on_cran()
  obj <- check.motuclient()
  expect_error(obj,NA)
})
