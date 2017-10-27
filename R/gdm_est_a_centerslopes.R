## File Name: gdm_est_a_centerslopes.R
## File Version: 0.01

gdm_est_a_centerslopes <- function(a, centerslopes, Qmatrix, TD )
{
	if (centerslopes){
		if (TD>1){
			m11 <- t( colSums( a[,,1] ) / colSums( Qmatrix ) )	
			a[,,1] <- a[,,1] / m11[ rep(1,I) , ]
		}
		if (TD==1){
			m11 <- t( colSums( a ) / colSums( Qmatrix ) )	
			a <- a / m11[ rep(1,I) , ]
		}						
	}
	return(a)
}
