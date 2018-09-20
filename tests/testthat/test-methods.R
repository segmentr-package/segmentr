context('methods')

test_that('calculate log_likelihood of existing type', {
  points <- rbind(1:7)

  mult_likelihood <- prod

  results <- list(segments=c(3, 5), log_likelihood=prod)
  class(results) <- "segmentr"

  likelihood <- predict.segmentr(results, points)
  expect_equal(likelihood, (1 * 2) + (3 * 4) + (5 * 6 * 7))
})
