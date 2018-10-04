context("methods")

simulate2.1 <- function(N) {
  X1 <- sample(1:2, N, replace = TRUE)
  X2 <- sample(1:2, N, replace = TRUE)
  X3 <- sample(1:2, N, replace = TRUE)
  X4 <- sample(1:2, N, replace = TRUE)
  X5 <- sample(1:2, N, replace = TRUE)
  X6 <- sample(1:2, N, replace = TRUE)
  X <- cbind(X1, X1 - X2, X2, X1 + X2, X1, X3, X3 - X4, X4, X3 + X4, X3, X5, X5 - X6, X6, X5 + X6, X5)
  X
}

test_that("calculate log_likelihood of existing type", {
  points <- rbind(1:7)

  mult_likelihood <- prod

  results <- list(segments = c(3, 5), log_likelihood = prod)
  class(results) <- "segmentr"

  likelihood <- predict.segmentr(results, points)
  expect_equal(likelihood, (1 * 2) + (3 * 4) + (5 * 6 * 7))
})

test_that("works with segment function on both algorithms", {
  set.seed(1234)
  data <- simulate2.1(2000)
  results <- segment(data, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)))
  likelihood <- predict(results, data)
  expect_equal(likelihood, -11087.332, tolerance = 0.1)

  results <- segment(data, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), algorithm = "hieralg")
  likelihood <- predict(results, data)
  expect_equal(likelihood, -11087.332, tolerance = 0.1)
})
