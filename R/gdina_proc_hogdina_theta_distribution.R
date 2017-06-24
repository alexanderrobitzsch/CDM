
gdina_proc_hogdina_theta_distribution <- function(G)
{
	reduced.skillspace <- FALSE
	theta.k <- seq( -6,6 , len=21 )
	wgt.theta <- stats::dnorm( theta.k )
	w1 <- wgt.theta / sum( wgt.theta )
	wgt.theta <- matrix( w1 , nrow=length(w1) , ncol=G)
	#--- OUTPUT
	res <- list(theta.k=theta.k, reduced.skillspace=reduced.skillspace, wgt.theta=wgt.theta)
	return(res)
}	
