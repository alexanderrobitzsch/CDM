## File Name: gdina_calc_ic.R
## File Version: 0.092

gdina_calc_ic <- function( delta, delta.designmatrix, delta.fixed, G, ncolZ, K, HOGDINA,
        item.patt.freq, zeroprob.skillclasses, loglike, numb_regular_pars,
        attr.prob.fixed=NULL)
{
    bb <- 0
    Nipar <- length( unlist(delta) )
    if ( ! is.null( delta.designmatrix ) ){
        Nipar <- ncol(delta.designmatrix )
    }
    if ( ! is.null( delta.fixed) ){
        Nipar <- Nipar - sum(1 - is.na( unlist( delta.fixed )) )
    }
    if ( ! is.na(numb_regular_pars) ){
        Nipar <- Nipar - numb_regular_pars
    }
    Nskillpar <- G*ncolZ - length( zeroprob.skillclasses )
    if (!is.null(attr.prob.fixed)){
        Nskillpar <- 0
    }
    if (HOGDINA==1){ Nskillpar <- 2*K*G }
    if (HOGDINA==0){ Nskillpar <- K*G }
    Npars <- Nipar  - bb + Nskillpar
    II <- sum( item.patt.freq )
    aic <- -2*loglike + 2 * Npars
    bic <- -2*loglike + Npars*log(II)
    caic <- -2*loglike + ( log(II) + 1 ) * Npars
    #*** create object ic
    ic <- list(deviance=-2*loglike, AIC=aic, BIC=bic, CAIC=caic)
    #---- OUTPUT
    res <- list(Npars=Npars, aic=aic, bic=bic, caic=caic, Nskillpar=Nskillpar,
                        Nipar=Nipar, ic=ic)
    return(res)
}
