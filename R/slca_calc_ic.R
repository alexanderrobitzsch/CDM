## File Name: slca_calc_ic.R
## File Version: 0.13
## File Last Change: 2017-10-04 17:22:04


#############################################################
# calculation of information criteria and number of parameters
slca_calc_ic <- function( dev, dat, G, K, TP ,I , delta.designmatrix , delta.fixed ,
			Xlambda , Xlambda.fixed , data0 , deltaNULL , Xlambda.constr.V 	)
{
    ic <- list( "deviance" = dev , "n" = nrow(data0) )
	ic$traitpars <- 0
	ic$itempars <- 0	
	
	ic$itempars <- length(Xlambda)
	if ( ! is.null(Xlambda.fixed ) ){
		ic$itempars <- ic$itempars - nrow(Xlambda.fixed )
	}
	if ( ! is.null( Xlambda.constr.V ) ){
		ic$itempars <- ic$itempars - ncol(Xlambda.constr.V )
	}
																
	ic$traitpars <- G * ncol(delta.designmatrix ) - G*deltaNULL
	if ( ! is.null(delta.fixed ) ){
		ic$traitpars <- ic$traitpars - nrow(delta.fixed )
	}
	#***********************************************
	# information criteria
	ic$np <- ic$itempars + ic$traitpars	
	#-- compute criteria
	ic <- cdm_calc_information_criteria(ic=ic)		
    return(ic)
}
###################################################################
		
.slca.calc.ic <- slca_calc_ic
