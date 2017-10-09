## File Name: gdm_est_a_constraints.R
## File Version: 0.01
## File Last Change: 2017-10-08 17:02:35

gdm_est_a_constraints <- function(a, se.a, a.constraint, increment )
{
	if ( ! is.null( a.constraint) ){
		a[ a.constraint[,1:3,drop=FALSE] ] <- a.constraint[,4,drop=FALSE]
		se.a[ a.constraint[,1:3,drop=FALSE] ] <- 0			
		increment[ a.constraint[,1:2,drop=FALSE] ] <- 0			
	}	
	#--- output
	res <- list( a=a, se.a=se.a, increment=increment )
	return(res)
}
