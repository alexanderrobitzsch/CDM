## File Name: reglca_scad_threshold.R
## File Version: 0.03

reglca_scad_threshold <- function(beta, lambda, a=3.7)
{
	sign_beta <- sign(beta)
	#-- 2*lambda < abs(beta) < a*lambda
	y <- ( ( a - 1 ) * beta - sign_beta * a * lambda ) / ( a - 2 )
	#-- abs(beta) > a*lambda
	y <- ifelse( abs(beta) > a*lambda , beta , y )
	#-- abs(beta) < 2*lambda
	z <- ( abs(beta) - lambda )
	z <- ifelse( z < 0 , 0 , z )
	y <- ifelse( abs(beta) < 2*lambda , z*sign_beta, y )
	return(y)
}
