## File Name: reglca_calc_deviance.R
## File Version: 0.02

reglca_calc_deviance <- function( p.xi.aj , class_probs, weights, loglike, 
			penalty=0, opt_fct=0 )
{
	eps <- 1E-30
	p.xi.aj[ p.xi.aj > 1 ] <- 1 - eps
	p.xi.aj[ p.xi.aj < 0 ] <- eps	
	N <- nrow(p.xi.aj)
	l1 <- rowSums( p.xi.aj * cdm_matrix2( class_probs , nrow=N )  ) + eps
	l1[ l1 < 0 ] <- eps

	like.new <- sum( log( l1 ) * weights) 
    likediff <- abs( loglike - like.new )
	
	#--- regularization
	opt_fct_old <- opt_fct
	opt_fct <- -2*like.new + 2* penalty
	opt_fct_change <- - opt_fct + opt_fct_old
	
	#--- OUTPUT
	res <- list( like.new=like.new, likediff=likediff, opt_fct = opt_fct,
					opt_fct_change=opt_fct_change)
	return(res)
}
