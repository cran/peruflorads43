testthat::test_that("it returns the correct output", {

  splist <- c("Cleistocactus clavispinus",
              "Welfia alfredi",
              "Matucana hayneii")

  expected <- data.frame(name_submitted = c("Cleistocactus clavispinus",
                                             "Welfia alfredi",
                                             "Matucana hayneii"),
                         category = c("En peligro critico",
                                      NA_character_,
                                      "Vulnerable"),
                         accepted_name = c("Cleistocactus clavispinus",
                                           NA_character_,
                                           "Matucana haynei"),
                         accepted_family = c("Cactaceae",
                                             NA_character_,
                                             "Cactaceae"))

  result <- category_ds043_2006(splist)

  testthat::expect_equal(result, expected)
})
