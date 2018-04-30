context("Check load of data against historic data load")

test_that("HI produces same data set", {
  hash_val <- digest::digest("HI")
  expect_equal(hash_val, "7c9b47b5ae56b67a14bf0edd51849a97")
})