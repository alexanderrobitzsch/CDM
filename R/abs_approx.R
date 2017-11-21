## File Name: abs_approx.R
## File Version: 0.05


# quadratic approximation of the absolute value function
abs_approx <- function( x , eps = 1E-5)
{
	res <- sqrt( x^2 + eps )
	return(res)
}

