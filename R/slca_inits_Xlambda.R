## File Name: slca_inits_Xlambda.R
## File Version: 0.02
## File Last Change: 2017-10-06 10:46:06

slca_inits_Xlambda <- function( Xlambda.init, Xdes, Nlam)
{	
	if ( is.null( Xlambda.init ) ){
		Xlambda.init <- stats::runif( Nlam , -1 , 1 )
	}
	return(Xlambda.init)
}
