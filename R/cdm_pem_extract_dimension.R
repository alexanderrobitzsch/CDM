## File Name: cdm_pem_extract_dimension.R
## File Version: 0.01
## File Last Change: 2017-10-04 17:19:04

cdm_pem_extract_dimension <- function(x)
{
	x_dim <- NULL
	if ( is.vector(x) ){
		x_dim <- length(x)
	}
	if ( is.matrix(x) | is.array(x) | is.data.frame(x) ){
		x_dim <- dim(x)
	}
	return(x_dim)
}
