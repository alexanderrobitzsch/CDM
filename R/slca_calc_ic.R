## File Name: slca_calc_ic.R
## File Version: 0.22


#############################################################
# calculation of information criteria and number of parameters
slca_calc_ic <- function( dev, dat, G, K, TP ,I , delta.designmatrix , delta.fixed ,
            Xlambda , Xlambda.fixed , data0 , deltaNULL , Xlambda.constr.V,
            regularization, regular_indicator_parameters, Xlambda_positive)
{
    ic <- list( "deviance" = dev , "n" = nrow(data0) )
    ic$traitpars <- 0
    ic$itempars <- 0

    ic$itempars <- length(Xlambda)
    if ( ! is.null(Xlambda.fixed ) ){
        ic$itempars <- ic$itempars - nrow(Xlambda.fixed )
    }

    #--- count number of estimated parameters
    ind_regular <- ( Xlambda == 0 ) * regular_indicator_parameters
    ind_positive <- ( Xlambda == 0 ) * Xlambda_positive
    ind_nonactive <- 1 * ( ind_regular | ind_positive )
    ic$nonactive <- sum(ind_nonactive)
    ic$itempars <- ic$itempars - ic$nonactive

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
