# Function to calculate maximum loglikehood of a discrete multivariate distribution.
# Each *row* corresponds to a single observation.

#' @export
#' @
r_multivariate <- function(X, na.omit=TRUE)
{
  X <- as.matrix(X)

  if (na.omit) {
    X <- na.omit(X)
  }

  ip <- table( apply(as.matrix(X),1,paste0,collapse="") )
  n <- nrow(X)
  pip <- ip/sum(ip)
  loglik <-  sum( ip*log(pip) )
  df <- length(pip)
  loglik
}

#' @export
multivariate <- function(X, na_action=na.omit)
{
  X <- as.matrix(X)
  X <- na_action(X)
  cpp_multivariate(X)
}


# Function that implements the dynamic programming algorithm

#' @export
segment <- function(x,segmax=ncol(x),loglikfun=multivariate, penality = function(x) 0)
{
  m <- ncol(x)
  n <- nrow(x)
  z <- matrix(ncol=m,nrow=segmax)
  r <- matrix(ncol=m,nrow=segmax-1)
  for(i in 1:m){
    w <- loglikfun( as.matrix(x[,1:i]) )
    z[1,i] <- w - penality(x[, 1:i, drop=FALSE])
  }
  for(k in 2:segmax){
    for(i in k:m){
      q <- c()
      for(j in (k-1):(i-1)){
        w <- loglikfun( as.matrix(x[,(j+1):i, drop=FALSE]) )
        q <- c(q, z[k-1,j] + w - penality(x[, (j+1):i, drop=FALSE]))
      }
      z[k,i] <- max(q)
      r[k-1,i] <- which.max(q) + k - 2
    }
  }
  bic <- z[,m]
  segshat <- which.max(bic)
  if (segshat > 1){
    segs <- r[segshat-1,m]
    if( segshat > 2 ) for(k in (segshat-1):2) segs <- c(r[k-1,segs[1]],segs)
  }
  else segs <- c()
  segs
}

#' @export
hieralg <- function(x,ini=1,loglikfun=multivariate,alf=2,c1=0.5,c2=0.5)
{
  m <- ncol(as.matrix(x))
  n <- nrow(as.matrix(x))
  z <- c()
  if(m>1){
    for(i in 1:(m-1))
      z <- c(z,loglikfun(as.matrix(x[,1:i]))[1] - c1*alf^i*log(n)
             + loglikfun(as.matrix(x[,(i+1):m]))[1] - c1*alf^(m-i)*log(n)) + c2*log(n)
  }
  z <- c(z,loglikfun(as.matrix(x))[1] - c1*alf^m*log(n))
  k <- which.max(z)
  segs <- c()
  if( k < m ){
    segs <- k+ini-1
    k1 <- hieralg(as.matrix(x[,1:k]),ini,loglikfun,alf,c1,c2)
    if(length(k1) > 0) segs <- union(segs,k1)
    k2 <- hieralg(as.matrix(x[,(k+1):m]),ini=ini+k,loglikfun,alf,c1,c2)
    if(length(k2) > 0) segs <- union(segs,k2)
  }
  if (length(segs) > 0) segs <- sort(segs)
  segs
}
