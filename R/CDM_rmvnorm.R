## File Name: CDM_rmvnorm.R
## File Version: 0.02


CDM_rmvnorm <- function(n, mean=NULL, sigma, ...)
{
	add_means <- FALSE
	if ( missing(n) ){
		n <- nrow(mean)
		add_means <- TRUE
		mean0 <- mean
		mean <- rep(0,ncol(mean))
	}
	if (is.null(mean)){
		mean <- rep(0,ncol(sigma) )
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
