## File Name: gdina_mstep_item_ml_update_parameter.R
## File Version: 0.596

gdina_mstep_item_ml_update_parameter <- function( delta_jj, max_increment,
    regular_lam, regular_type, regularization, ll_FUN, h, mstep_conv, cd_steps,
    regular_alpha, regular_tau, N=NULL, regular_weights=NULL)
{
    eps <- 1E-5
    if (is.null(N)){ N <- 1 }
    #--- no regularization
    if ( ! regularization ){
        res1 <- numerical_Hessian(par=delta_jj, h=h, FUN=ll_FUN, gradient=TRUE, hessian=TRUE, diag_only=FALSE )
        grad <- res1$grad
        hessian <- - res1$hessian
        hessian <- cdm_add_ridge_diagonal(x=hessian, eps=eps )
        delta_change <- as.vector( MASS::ginv(hessian) %*% grad )
        delta_change <- cdm_trim_increment( increment=delta_change, max.increment=max_increment, type=2 )
        delta_jj <- delta_jj + delta_change
    }
    #--- regularization
    if (regularization){
        NP <- length(delta_jj)
        for (pp in 1:NP){
            iterate_pp <- TRUE
        # cat("---------- pp=", pp, "----------------\n")
            vv <- 0
            #- start iterations in coordinate descent algorithm
            while (iterate_pp){
                delta_jj_pp <- delta_jj
                res <- numerical_Hessian_partial(par=delta_jj, FUN=ll_FUN, h=h, coordinate=pp )
                grad <- res$grad
                hessian <- - res$hessian
                delta_change <- grad / ( abs(hessian) + eps )
                delta_jj[pp] <- delta_jj[pp] - delta_change
                vv <- vv + 1
                if (pp >=2){
                    x0 <- delta_jj[pp]
                    regular_lam_temp <- regular_lam
                    regular_tau_temp <- regular_tau
                    vt <- abs(hessian) / N
                    if ( ! is.null(regular_weights) ){
                        regular_lam_temp <- regular_lam_temp*regular_weights[pp]
                        regular_tau_temp <- regular_tau_temp*regular_weights[pp]
                    }
                    delta_jj[pp] <- gdina_mstep_item_ml_update_parameter_regularization(x=x0,
                                            regular_type=regular_type, regular_lam=regular_lam_temp,
                                            regular_alpha=regular_alpha, regular_tau=regular_tau_temp,
                                            vt=vt )
                }
                parchange_pp <- max( abs( delta_jj_pp - delta_jj ))
                if ( parchange_pp < mstep_conv ){ iterate_pp <- FALSE }
                if ( vv > cd_steps ){ iterate_pp <- FALSE }
            }  # end while iterate_pp
        } # end pp
    }
    #--- output
    return(delta_jj)
}
