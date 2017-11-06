## File Name: cdm_matrix2.R
## File Version: 0.01

cdm_matrix2 <- function( x , nrow )
{
	y <- matrix( x , nrow=nrow, ncol=length(x), byrow=TRUE )
	return(y)
}
