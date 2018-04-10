# Function to calculate maximum loglikehood of a discrete multivariate distribution.
# Each *row* corresponds to a single observation.

#' @example
r_multivariate <- function(X)
{
	ip <- table( apply(as.matrix(X),1,paste0,collapse="") )
	n <- nrow(X)
	pip <- ip/sum(ip)
	loglik <-  sum( ip*log(pip) )
	df <- length(pip)
	loglik
}


# Function that implements the dynamic programming algorithm

#' @export
segment <- function(x,segmax=ncol(x),loglikfun=multivariate,c1=1)
{
	m <- ncol(x)
	n <- nrow(x)
#	alf <- length(levels(factor(x)))
	z <- matrix(ncol=m,nrow=segmax)
	r <- matrix(ncol=m,nrow=segmax-1)
	for(i in 1:m){
		w <- loglikfun( as.matrix(x[,1:i]) )
		z[1,i] <- w[1] + c1*sqrt(n)# - c2*(w[2]-1)
	}
	for(k in 2:segmax){
		for(i in k:m){
			q <- c()
			for(j in (k-1):(i-1)){
				w <- loglikfun( as.matrix(x[,(j+1):i]) )
				q <- append(q, z[k-1,j] + w[1] + c1*sqrt(n))# - c2*(w[2]-1) )
			}
			z[k,i] <- max(q)
			r[k-1,i] <- which.max(q) + k - 2
		}
	}
#	print(z[,m])
	segshat <- which.max(z[,m])
	if (segshat > 1){
		segs <- r[segshat-1,m]
		if( segshat > 2 ) for(k in (segshat-1):2) segs <- c(r[k-1,segs[1]],segs)
	}
	else segs <- c()
	segs
}

#' @export
hieralg <- function(x,ini=1,loglikfun=multivariate,c=1)
{
  m <- ncol(x)
  n <- nrow(x)
  #	alf <- length(levels(factor(x)))
  z <- c()
  if(m>1){
    for(i in 1:(m-1))
      z <- c(z,loglikfun(as.matrix(x[,1:i]))[1]
             + loglikfun(as.matrix(x[,(i+1):m]))[1] + 2*c*sqrt(n))
  }
  z <- c(z,loglikfun(as.matrix(x))[1] + c*sqrt(n)	)
  k <- which.max(z)
  segs <- c()
  if( k < m ){
    segs <- k+ini-1
    k1 <- hieralg(as.matrix(x[,1:k]),ini,loglikfun,c)
    if(length(k1) > 0) segs <- union(segs,k1)
    k2 <- hieralg(as.matrix(x[,(k+1):m]),ini=ini+k,loglikfun,c)
    if(length(k2) > 0) segs <- union(segs,k2)
  }
  if (length(segs) > 0) segs <- sort(segs)
  segs
}
