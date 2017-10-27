## File Name: abs_approx.R
## File Version: 0.02

###############################################################
# quadratic approximation of the absolute value function
abs_approx <- function( x , eps = 1E-5){
	res <- sqrt( x^2 + eps )
	return(res)
}

abs_approx_D1 <- function( x , eps = 1E-5){
	res <-  x / sqrt( x^2 + eps )
	return(res)
}
