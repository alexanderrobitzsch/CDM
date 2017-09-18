## File Name: abs_approx.R
## File Version: 0.02
## File Last Change: 2017-01-31 14:07:25

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
