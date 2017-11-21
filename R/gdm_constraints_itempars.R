## File Name: gdm_constraints_itempars.R
## File Version: 0.02

###############################################
# constraints for item parameters
gdm_constraints_itempars <- function( b.constraint , a.constraint , 
	K , TD , Qmatrix , a )
{
	for (kk in 1:K){
		for( td in 1:TD){
			ind.kk <- which( Qmatrix[ ,td , kk] == 0 )
			a[ ind.kk , td , kk ] <- 0
			if ( length( ind.kk) > 0 ){
				a1 <- cbind( ind.kk  , td , kk , 0 )
				a.constraint <- rbind( a.constraint , a1 )
			}
		}
	}
	if ( ! is.null( a.constraint) ){
		a.constraint <- as.matrix( a.constraint )
	}
	res <- list( a.constraint = a.constraint , b.constraint=b.constraint , a=a)			
	return(res)
}
