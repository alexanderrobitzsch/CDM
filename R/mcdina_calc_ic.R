## File Name: mcdina_calc_ic.R
## File Version: 0.01

#################################################
# mcdina information criteria
mcdina_calc_ic <- function( dev, weights, itemstat, pi.k, G, I,
    zeroprob.skillclasses,  reduced.skillspace, Z )
{
    ic <- list( "deviance"=dev, "n"=sum(weights), "loglik"=-dev/2 )
    ic$G <- G
    ic$itempars <- sum( itemstat$N.pars)
    ic$traitpars <- G*(nrow(pi.k)-1 - length( zeroprob.skillclasses ) )
    if ( reduced.skillspace ){
        ic$traitpars <- G * ncol(Z)
    }
    ic$np <- ic$itempars + ic$traitpars
    ic$Nskillclasses <- nrow(pi.k) - length( zeroprob.skillclasses )
    # AIC
    ic$AIC <- dev + 2*ic$np
    # BIC
    ic$BIC <- dev + ( log(ic$n) )*ic$np
    # CAIC (consistent AIC)
    ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
    # corrected AIC
    ic$AICc <- ic$AIC + 2*ic$np * ( ic$np + 1 ) / ( ic$n - ic$np - 1 )
    return(ic)
}
