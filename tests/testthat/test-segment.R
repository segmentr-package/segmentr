context("segment")
require("glue")

segments <- list(1:10, 11:14, 15:28)
data <- make_segmented(segments)

test_that("consistently identify results", {
  for (algorithm in c("exact", "hierarchical", "hybrid")) {
    results <- segment(data, log_likelihood = mean_likelihood, penalty = function(X) 1, algorithm = algorithm)
    expect_equal(results$changepoints, c(10, 14))
    expect_equal(results$segments, segments)
  }
})

test_that("shows correct representation", {
  for (algorithm in c("exact", "hierarchical", "hybrid")) {
    results <- segment(data, log_likelihood = mean_likelihood, penalty = function(X) 1, algorithm = algorithm)
    print_value <- capture_print(results)
    expected_value <- capture_print(glue("
    Segments (total of 3):

    1:10
    11:14
    15:28
    "))
    expect_equal(print_value, expected_value)
  }
})
