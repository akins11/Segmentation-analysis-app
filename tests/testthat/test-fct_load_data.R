
test_that("check_for_missing_values() return TRUE when there is a missing value in the data & FALSE if There isn't", {
  testthat::local_edition(3)

  with_na <- data.frame(xx = c(45, 78, 58, NA), yy = c("ff", "tt", NA, NA), zz = 10:13)

  without_na <- data.frame(xx = 45:50, yy = letters[1:6])


  expect_true(check_for_missing_values(with_na))

  expect_false(check_for_missing_values(without_na))

})



test_that("drop_all_missing_values() drops all missing value", {
  testthat::local_edition(3)

  with_na <- data.frame(xx = c(45, 78, 58, NA), yy = c("ff", "tt", NA, NA), zz = 10:13)

  expect_false(check_for_missing_values(drop_all_missing_values(with_na)))
})

# drop_all_missing_values() return a vector when only on column is avaliable.
# test_that("drop_all_missing_values() drops all columns with missing value based on the missing_threshold", {
#   testthat::local_edition(3)
#
#   with_na <- data.frame(xx = c(45, 78, NA, NA), yy = c("ff", NA, NA, NA), zz = 10:13)
#
#   # with >= 50% --------------------------------------------------------------->
#   expect_equal(1, drop_all_missing_values(with_na, 50) |> ncol())
#
#   # with <= 70% --------------------------------------------------------------->
#   expect_equal(2, drop_all_missing_values(with_na, 70) |> ncol())
#
#   # with 100% ----------------------------------------------------------------->
#   expect_equal(3, drop_all_missing_values(with_na, 100) |> ncol())
# })


