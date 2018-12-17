require('microbenchmark')
require('profvis')

makeRandom <- function(rows, columns) {
  matrix(rbinom(rows * columns, size = 2, p=0.5), nrow=rows, ncol=columns)
}

segments_1to5_6to10_11to15 = function(N)
{
  X1 = sample(1:2,N,replace=TRUE)
  X2 = sample(1:2,N,replace=TRUE)
  X3 = sample(1:2,N,replace=TRUE)
  X4 = sample(1:2,N,replace=TRUE)
  X5 = sample(1:2,N,replace=TRUE)
  X6 = sample(1:2,N,replace=TRUE)
  X = cbind(X1,X1-X2,X2,X1+X2,X1,X3,X3-X4,X4,X3+X4,X3,X5,X5-X6,X6,X5+X6,X5)
  X
}

set.seed(123)
data <- segments_1to5_6to10_11to15(5000)

microbenchmark(
  segment(data, likelihood = multivariate),
  segment(data, likelihood = r_multivariate),
  hieralg(data, likelihood = multivariate),
  hieralg(data, likelihood = r_multivariate),
  times = 5
)


data <- matrix(rbinom(10000 * 10, size = 2, p=0.5), nrow=10000, ncol=100)
doMC::registerDoMC(4)
microbenchmark(
  segment(data, likelihood = multivariate, allow_parallel = FALSE),
  segment(data, likelihood = multivariate, allow_parallel = TRUE),
  hieralg(data, likelihood = multivariate, allow_parallel = FALSE),
  hieralg(data, likelihood = multivariate, allow_parallel = TRUE),
  times = 1
)

data <- makeRandom(100, 1000)
doMC::registerDoMC(4)
microbenchmark(
  hieralg(data, likelihood = multivariate, allow_parallel = FALSE),
  hieralg(data, likelihood = multivariate, allow_parallel = TRUE),
  times = 2
)

data <- makeRandom(100, 1000)
profvis(hieralg(data, likelihood = multivariate, allow_parallel = FALSE))
