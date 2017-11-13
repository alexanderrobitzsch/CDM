## File Name: cdm_matrix2.R
## File Version: 0.02

cdm_matrix2 <- function( x , nrow )
{
	x <- as.vector(x)
	y <- matrix( x , nrow=nrow, ncol=length(x), byrow=TRUE )
	return(y)
}
