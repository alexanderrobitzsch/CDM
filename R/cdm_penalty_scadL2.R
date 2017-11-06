## File Name: cdm_penalty_scadL2.R
## File Version: 0.01

cdm_penalty_scadL2 <- function( x, lambda, alpha )
{
	lam1 <- lambda * alpha
	lam2 <- lambda * ( 1 - alpha )
	y <- cdm_penalty_scad(x=x, lambda=lam1) + lam2*x^2
	return(y)
}
