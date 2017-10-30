## File Name: cdm_penalty_lasso.R
## File Version: 0.04

cdm_penalty_lasso <- function( x, lambda )
{
	y <- lambda * abs(x)
	return(y)
}
