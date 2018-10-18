context("hybridalg")

global_threshold <- 10
set.seed(123)
data_1 <- simulate2.1(2000)
data_2 <- simulate3.2(5000)

test_that("correctly identify independent results", {
  results <- hybridalg(data_1, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), threshold = global_threshold)
  expect_equal(results$changepoints, c(5, 10))

  data <- simulate3.2(5000)
  results <- hybridalg(data_2, penalty = function(X) (0.2 * 3^ncol(X)) * log(nrow(X)), threshold = global_threshold)
  expect_equal(results$changepoints, c(3, 7))
})

test_that("can be called using segment", {
  results <- segment(data_1, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), threshold = global_threshold, algorithm = "hybrid")
  expect_equal(results$changepoints, c(5, 10))

  results <- segment(data_2, penalty = function(X) (0.2 * 3^ncol(X)) * log(nrow(X)), algorithm = "hybrid", threshold = global_threshold)
  expect_equal(results$changepoints, c(3, 7))
})

test_that("works with cluster", {
  doParallel::registerDoParallel(1)
  results <- hybridalg(data_1, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), allow_parallel = TRUE, threshold = global_threshold)
  expect_equal(results$changepoints, c(5, 10))

  results <- hybridalg(data_2, penalty = function(X) (0.2 * 3^ncol(X)) * log(nrow(X)), allow_parallel = FALSE, threshold = global_threshold)
  expect_equal(results$changepoints, c(3, 7))
  doParallel::stopImplicitCluster()
})

test_that("handles corner cases", {
  data <- makeRandom(1000, 0)
  results <- hybridalg(data, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), allow_parallel = FALSE, threshold = global_threshold)
  expect_equal(length(results$changepoints), 0)
})

test_that("works with max_segments", {
  results <- hybridalg(data_1, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), allow_parallel = TRUE, max_segments = 2)
  expect_equal(results$changepoints, c(10))
})

test_that("handles NaN in log_likelihood or penalty", {
  set.seed(1234)
  data <- makeRandom(5, 20)
  expect_error(
    hybridalg(data, log_likelihood = function(X) if (ncol(X) == 2) NaN else sum(X)),
    "log_likelihood returned a NaN when called with log_likelihood\\(data\\[, 2:3\\]\\)"
  )

  expect_error(
    hybridalg(data, penalty = function(X) if (ncol(X) == 2) NaN else sum(X)),
    "penalty returned a NaN when called with penalty\\(data\\[, 2:3\\]\\)"
  )
})
