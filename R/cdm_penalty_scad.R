## File Name: cdm_penalty_scad.R
## File Version: 0.12

cdm_penalty_scad <- function( x, lambda, a = 3.7 )
{
	y <- lambda * abs(x)
	y <- ifelse( ( abs(x) > lambda ) & ( abs(x) < a*lambda ) , 
					-( x^2 - 2*a*lambda*abs(x) + lambda^2 )/ 2 / ( a-1) ,   y )
	y <- ifelse(  abs(x) > a*lambda ,  (a+1) * lambda^2 / 2 , y )
	return(y)
}
