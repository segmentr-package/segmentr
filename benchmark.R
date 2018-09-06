require('microbenchmark')

simulate2.1 = function(N)
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
data <- simulate2.1(5000)

microbenchmark(
  segment(data, log_likelihood = multivariate),
  segment(data, log_likelihood = r_multivariate),
  hieralg(data, log_likelihood = multivariate),
  hieralg(data, log_likelihood = r_multivariate),
  times = 5
)

doParallel::registerDoParallel(4)

data <- simulate2.1(10000)
microbenchmark(
  segment(data, log_likelihood = multivariate, allow_parallel = FALSE),
  segment(data, log_likelihood = multivariate, allow_parallel = TRUE),
  hieralg(data, log_likelihood = multivariate, allow_parallel = FALSE),
  hieralg(data, log_likelihood = multivariate, allow_parallel = TRUE),
  times = 5
)

data <- simulate2.1(50000)
microbenchmark(
  hieralg(data, log_likelihood = multivariate, allow_parallel = FALSE),
  hieralg(data, log_likelihood = multivariate, allow_parallel = TRUE),
  times = 5
)
doParallel::stopImplicitCluster()
