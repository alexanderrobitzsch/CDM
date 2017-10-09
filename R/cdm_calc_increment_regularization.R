## File Name: cdm_calc_increment_regularization.R
## File Version: 0.01
## File Last Change: 2017-10-07 19:49:17

cdm_calc_increment_regularization <- function( d1, d2, x0, regular_lam_used, max.increment, eps=1E-10, adj_fac=.98 )
{
	val <-  d2 * x0 + d1
	eta <- regular_lam_used
	res <- cdm_soft_threshold( val=val, eta=regular_lam_used )
	updated <- res / abs( d2 + eps )
	increment <- updated - x0
	increment[ is.na(increment) ] <- 0
	increment <- cdm_trim_increment( increment=increment, max.increment=max.increment ) 
	updated <- x0 + increment
	max.increment <- max(abs(increment)) / adj_fac
	#--- output
	res <- list( increment = increment, updated = updated, max.increment=max.increment )
	return(res)
}
	
