testthat::test_that("search_ds043 returns a character vector", {
  splist <- c("Cleistocactus clavispinus",
              "Welfia alfredi",
              "Matucana hayneii")
  testthat::expect_type(search_ds043(splist), "character")
})

testthat::test_that("search_ds043 returns the correct accepted names", {
  splist <- c("Cleistocactus clavispinus",
              "Welfia alfredi",
              "Matucana hayneii")
  testthat::expect_equal(search_ds043(splist),
                         c("Present", "", "P_updated_name"))
})
