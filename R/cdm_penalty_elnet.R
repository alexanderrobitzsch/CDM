## File Name: cdm_penalty_elnet.R
## File Version: 0.01

cdm_penalty_elnet <- function( x, lambda, alpha )
{
	lam1 <- lambda * alpha
	lam2 <- lambda * ( 1 - alpha )
	y <- lam1 * abs(x) + lam2 * x^2
	return(y)
}
