## File Name: reglca_update_parameter.R
## File Version: 0.24


reglca_update_parameter <- function(parm, pp, C, W, h, lambda, regular_type, cd_steps, conv, max_increment )
{

    iterate <- TRUE
    iter <- 0
    parchange <- 1
    while (iterate){
        parm_old <- parm
        probs0 <- reglca_calc_probs(parm=parm)
        # first derivative
        q0 <- reglca_freq_ll( x=probs0, C=C, W=W )
        parm1 <- parm
        parm1[pp] <- parm[pp] + h
        probs1 <- reglca_calc_probs(parm=parm1)
        q1 <- reglca_freq_ll( x=probs1, C=C, W=W )
        # second derivative
        parm1 <- parm
        parm1[pp] <- parm[pp] - h
        probs1 <- reglca_calc_probs(parm=parm1)
        q2 <- reglca_freq_ll( x=probs1, C=C, W=W )
        # differential quotients
        res <- cdm_ll_numerical_differentiation(ll0=q0, ll1=q1, ll2=q2, h=h)
        f1 <- res$d1
        f2 <- res$d2
        incr <- - f1 / f2
        # incr <- f1 / f2
        incr <- cdm_trim_increment( increment=incr, max.increment=max_increment, type=1)
        max_increment <- min( .10, max( abs(incr) ) / 1.02 )
        parm[pp] <- parm[pp] + incr

        #-- apply threshold operator
        if (pp>1){
            parm[pp] <- cdm_parameter_regularization(x=parm[pp], regular_type=regular_type, regular_lam=lambda)
        }
        iter <- iter + 1
        if ( iter > cd_steps ){ iterate <- FALSE }
        parchange <- max( abs( parm[pp] - parm_old[pp] ))
        if ( parchange < conv ){ iterate <- FALSE }

    }

    #-- output
    return(parm)
}
