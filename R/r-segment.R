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
	c(loglik,df)
}


# Function that implements the dynamic programming algorithm

#' @export
r_segment <- function(x,segmax=ncol(x),loglikfun=r_multivariate,c1=1)
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
segment <- function(x, loglikfun="multivariate")
{
  if (loglikfun %in% c("multivariate")) {
    segment_base(x, loglikfun, identity) + 2
  } else if (typeof(loglikfun) == "closure") {
    segment_base(x, "r_function", loglikfun) + 2
  } else {
    stop("invalid loglikfun")
  }
}

