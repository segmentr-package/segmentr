context("segment")

test_that("consistently identify results", {
  segments <- list(1:10, 11:14, 15:28)
  data <- make_segmented(segments)

  for (algorithm in c("exact", "hierarchical", "hybrid")) {
    results <- segment(data, log_likelihood = mean_likelihood, penalty = function(X) 1, algorithm = algorithm)
    expect_equal(results$changepoints, c(10, 14))
    expect_equal(results$segments, segments)
  }
})
