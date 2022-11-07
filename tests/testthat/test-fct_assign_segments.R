test_that("get_user_segments() returns a character vactor", {
  # Correct input
  expect_equal(get_user_segments("High, Middle, Low"),
               c("High", "Middle", "Low"))

  # Wrong input
  expect_equal(get_user_segments("High, Middle Low"),
               c("High", "Middle Low"))

  expect_equal(get_user_segments("High Middle Low"),
               c("High Middle Low"))
})



