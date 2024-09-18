library(testthat)
library(stratifiedyh)

test_that("stratified_labels assigns labels correctly", {
  # Create a sample dataframe
  df <- data.frame(x = 1:10, group = rep(c("A", "B"), each = 5))

  # Apply the function with the required group_col and yes_percentage arguments
  result <- stratified_labels(df, group_col = "group", yes_percentage = 50)

  # Check if the new column is created
  expect_true("Sampled_Yes_No" %in% names(result))

  # Check if the labels are correctly assigned as "Yes" or "No"
  expect_true(all(result$Sampled_Yes_No %in% c("Yes", "No")))
})
