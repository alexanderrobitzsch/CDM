## File Name: cdm_penalty_ridge.R
## File Version: 0.01

cdm_penalty_ridge <- function( x, lambda )
{
	y <- lambda * x^2
	return(y)
}
