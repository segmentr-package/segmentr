context("exactalg")

set.seed(123)
data_1 <- segments_1to5_6to10_11to15(1000)
data_2 <- segments_1to10_11to15(1000)

test_that("correctly identify independent results", {
  results <- exactalg(data_1, likelihood = function(X) multivariate(X) - (0.5 * 2^ncol(X)) * log(nrow(X)))
  expect_equal(results$segments, list(1:5, 6:10, 11:15))

  results <- exactalg(data_2, likelihood = function(X) multivariate(X) - (0.5 * 3^ncol(X)) * log(nrow(X)))
  expect_equal(results$segments, list(1:5, 6:10, 11:15))
})

test_that("can be called using segment", {
  results <- segment(data_1, likelihood = function(X) multivariate(X) - (0.5 * 2^ncol(X)) * log(nrow(X)))
  expect_equal(results$segments, list(1:5, 6:10, 11:15))

  results <- segment(data_2, likelihood = function(X) multivariate(X) - (0.5 * 3^ncol(X)) * log(nrow(X)), algorithm = "exact")
  expect_equal(results$segments, list(1:5, 6:10, 11:15))
})

test_that("works with cluster", {
  set.seed(123)
  data_1 <- segments_1to5_6to10_11to15(1000)
  data_2 <- segments_1to10_11to15(1000)

  doParallel::registerDoParallel(1)
  results <- exactalg(data_1, likelihood = function(X) multivariate(X) - (0.5 * 2^ncol(X)) * log(nrow(X)), allow_parallel = TRUE)
  expect_equal(results$segments, list(1:5, 6:10, 11:15))

  results <- exactalg(data_2, likelihood = function(X) multivariate(X) - (0.5 * 3^ncol(X)) * log(nrow(X)), allow_parallel = FALSE)
  expect_equal(results$segments, list(1:5, 6:10, 11:15))
  doParallel::stopImplicitCluster()
})

test_that("handles zero columns", {
  data <- makeRandom(1000, 0)
  results <- exactalg(data, likelihood = function(X) multivariate(X) - (0.1 * 2^ncol(X)) * log(nrow(X)))
  expect_equal(length(results$segments), 0)
})

test_that("handles NaN in likelihood", {
  data <- makeRandom(5, 20)
  expect_error(
    exactalg(data, likelihood = function(X) if (ncol(X) == 2) NaN else sum(X)),
    "likelihood returned a NaN when called with likelihood\\(data\\[, 2:3\\]\\)"
  )
})

test_that("test max segments", {
  results <- exactalg(data_1, likelihood = function(X) multivariate(X) - (0.5 * 2^ncol(X)) * log(nrow(X)), max_segments = 2)
  expect_equal(results$segments, list(1:9, 10:15))
})

test_that("fixes but with order of segments", {
  data <- rbind(
    c(1, 1, 0, 0, 0, 1023, 134521, 12324),
    c(1, 1, 0, 0, 0, -20941, 1423, 14334),
    c(1, 1, 0, 0, 0, 2398439, 1254, 146324),
    c(1, 1, 0, 0, 0, 24134, 1, 15323),
    c(1, 1, 0, 0, 0, -231, 1256, 13445),
    c(1, 1, 0, 0, 0, 10000, 1121, 331)
  )

  results <- segment(
    data,
    algorithm = "exact",
    likelihood = function(X) multivariate(X) - 0.01 * exp(ncol(X))
  )

  expect_equal(results$segments, list(1, 2, 3, 4, 5, 6:8))
})
