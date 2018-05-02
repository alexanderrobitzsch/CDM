## File Name: gdina_mstep_item_ml_algorithm.R
## File Version: 0.19

gdina_mstep_item_ml_algorithm <- function(delta_jj, max_increment,
                regular_lam, regular_type, regularization, ll_FUN, h, mstep_conv, cd_steps,
                mstep_iter, regular_alpha, regular_tau )
{
    ii <- 0

    converged <- FALSE
    while ( ! converged ){
        delta_jj0 <- delta_jj
        #-- parameter update
        delta_jj <- gdina_mstep_item_ml_update_parameter( delta_jj=delta_jj,
                            max_increment=max_increment, regular_lam=regular_lam,
                            regular_type=regular_type, regularization=regularization,
                            ll_FUN=ll_FUN, h=h, mstep_conv=mstep_conv, cd_steps=cd_steps,
                            regular_alpha = regular_alpha, regular_tau=regular_tau )
        ii <- ii + 1
        decr <- max( abs( delta_jj - delta_jj0) )
        if ( ii >= mstep_iter ){  converged <- TRUE    }
        if ( decr < mstep_conv ){  converged <- TRUE }
    }   # ----- end algorithm

    #---- output
    return(delta_jj)
}
