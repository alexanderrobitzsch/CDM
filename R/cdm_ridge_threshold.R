## File Name: cdm_ridge_threshold.R
## File Version: 0.02

cdm_ridge_threshold <- function(beta, lambda)
{
	y <- beta / ( 1 + 2*lambda )
	return(y)
}
