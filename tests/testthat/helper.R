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

makeRandom <- function(rows, columns) {
  matrix(rbinom(rows * columns, size = 2, p = 0.5), nrow = rows, ncol = columns)
}
