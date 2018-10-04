context("hybridalg")

global_threshold <- 10

makeRandom <- function(rows, columns) {
  matrix(rbinom(rows * columns, size = 2, p = 0.5), nrow = rows, ncol = columns)
}

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

simulate3.2 <- function(N) {
  X1 <- sample(1:3, N, replace = TRUE)
  X2 <- sample(1:3, N, replace = TRUE)
  X3 <- X1
  X4 <- X2
  X5 <- sample(1:3, N, replace = TRUE)
  X6 <- sample(1:3, N, replace = TRUE)
  X <- cbind(X1, X1 - X2, X2, X1 + X2, X1, X3, X3 - X4, X4, X3 + X4, X3, X5, X5 - X6, X6, X5 + X6, X5)
  X
}


test_that("correctly identify independent results", {
  set.seed(123)
  data <- simulate2.1(2000)
  results <- hybridalg(data, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), threshold = global_threshold)
  expect_equal(results$segments, c(5, 10))

  data <- simulate3.2(5000)
  results <- hybridalg(data, penalty = function(X) (0.2 * 3^ncol(X)) * log(nrow(X)), threshold = global_threshold)
  expect_equal(results$segments, c(3, 7))
})

test_that("can be called using segment", {
  set.seed(123)
  data_1 <- simulate2.1(2000)
  data_2 <- simulate3.2(5000)

  results <- segment(data_1, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), threshold = global_threshold, algorithm = "hybrid")
  expect_equal(results$segments, c(5, 10))

  results <- segment(data_2, penalty = function(X) (0.2 * 3^ncol(X)) * log(nrow(X)), algorithm = "hybrid", threshold = global_threshold)
  expect_equal(results$segments, c(3, 7))
})

test_that("works with cluster", {
  set.seed(123)
  data_1 <- simulate2.1(2000)
  data_2 <- simulate3.2(5000)

  doParallel::registerDoParallel(1)
  results <- hybridalg(data_1, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), allow_parallel = TRUE, threshold = global_threshold)
  expect_equal(results$segments, c(5, 10))

  results <- hybridalg(data_2, penalty = function(X) (0.2 * 3^ncol(X)) * log(nrow(X)), allow_parallel = FALSE, threshold = global_threshold)
  expect_equal(results$segments, c(3, 7))
  doParallel::stopImplicitCluster()
})

test_that("handles corner cases", {
  set.seed(1234)
  data <- makeRandom(1000, 0)
  results <- hybridalg(data, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), allow_parallel = FALSE, threshold = global_threshold)
  expect_equal(results$segments, c())
})

test_that("works with max_segments", {
  set.seed(1234)
  data_1 <- simulate2.1(2000)

  results <- hybridalg(data_1, penalty = function(X) (0.1 * 2^ncol(X)) * log(nrow(X)), allow_parallel = TRUE, max_segments = 2)
  expect_equal(results$segments, c(5))
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
