## File Name: cdm_add_ridge_diagonal.R
## File Version: 0.01
## File Last Change: 2017-10-08 14:43:36

cdm_add_ridge_diagonal <- function(x , eps=1E-10 )
{
	D <- ncol(x)
	rx <- x + diag( eps , D )
	return(rx)
}
	
