## File Name: CDM_rmvnorm.R
## File Version: 0.01
## File Last Change: 2017-07-12 10:41:43


CDM_rmvnorm <- function(n, mean, sigma, ...)
{
	add_means <- FALSE
	if ( missing(n) ){
		n <- nrow(mean)
		add_means <- TRUE
		mean0 <- mean
		mean <- rep(0,ncol(mean))
	}
	x <- mvtnorm::rmvnorm(n=n, mean=mean, sigma=sigma, ...)
	if (n==1){
		x <- as.vector(x)
	}
	if (add_means){
		x <- x + mean0
	}
	return(x)
}
