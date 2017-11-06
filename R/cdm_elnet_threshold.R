## File Name: cdm_elnet_threshold.R
## File Version: 0.04

cdm_elnet_threshold <- function( beta, lambda, alpha )
{
	lam1 <- lambda * alpha
	lam2 <- lambda * ( 1 - alpha )
	res <- cdm_soft_threshold( val=beta, eta=lam1 )
	res <- res / ( 1 + 2*lam2 )
	return(res)
}
