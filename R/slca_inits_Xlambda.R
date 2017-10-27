## File Name: slca_inits_Xlambda.R
## File Version: 0.03

slca_inits_Xlambda <- function( Xlambda.init, Xdes, Nlam, Xlambda_positive, Xlambda.fixed)
{	
	if ( is.null( Xlambda_positive ) ){
		Xlambda_positive <- rep( FALSE, Nlam )
	}
	if ( is.null( Xlambda.init ) ){
		Xlambda.init <- stats::runif( Nlam , -1 , 1 ) + 1 * Xlambda_positive
	}
	if ( ! is.null( Xlambda.fixed ) ){
		Xlambda.init[ Xlambda.fixed[,1] ] <- Xlambda.fixed[,2]
		Xlambda_positive[ Xlambda.fixed[,1] ] <- FALSE	
	}
	#--- output
	res <- list( Xlambda.init=Xlambda.init, Xlambda_positive=Xlambda_positive)
	return(res)
}
