## File Name: gdina_postproc_regularized_constrained_parameters.R
## File Version: 0.14

gdina_postproc_regularized_constrained_parameters <- function( mono.constr, delta, Aj_mono_constraints,
        Mj, linkfct, regularization, data )
{
    #--- number of boundary estimates for monotonicity constraint
    numb_bound_mono <- NA
    J <- length(delta)
    item_bound_mono <- NULL
    if ( mono.constr ){
        eps_squeeze <- 1E-5
        eps <- 1E-3
        numb_bound_mono <- 0
        for (jj in 1:J){
            delta_jj <- delta[[jj]]
            Aj_mono_constraints_jj <- Aj_mono_constraints[[jj]]
            Mjjj <- Mj[[jj]][[1]]
            irf1 <- gdina_prob_item_designmatrix( delta_jj=delta_jj, Mjjj=Mjjj, linkfct=linkfct, eps_squeeze=eps_squeeze )
            constraints_fitted_jj <- as.vector( Aj_mono_constraints_jj %*% irf1 )
            indi_bound <- any( constraints_fitted_jj < eps )
            if (indi_bound){
                numb_bound_mono <- numb_bound_mono + indi_bound
                item_bound_mono <- c( item_bound_mono, colnames(data)[jj] )
            }
        }
    }
    #--- regularized parameters
    numb_regular_pars <- NA
    if (regularization){
        numb_regular_pars <- 0
        eps <- 1E-4
        for (jj in 1:J){
            delta_jj <- delta[[jj]]
            numb_regular_pars <- numb_regular_pars + sum( abs( delta_jj ) < eps )
        }
    }
    #--- output
    res <- list( numb_bound_mono=numb_bound_mono, numb_regular_pars=numb_regular_pars,
                    item_bound_mono=item_bound_mono)
    return(res)
}
