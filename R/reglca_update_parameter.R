## File Name: reglca_update_parameter.R
## File Version: 0.574


reglca_update_parameter <- function(parm, pp, C, W, h, lambda, regular_type,
    cd_steps, conv, max_increment, vt=NULL, prob_min=0, increment_factor=1.02,
    ii=NULL, eps=1e-8)
{
    iterate <- TRUE
    iter <- 0
    parchange <- 1
    vt_null <- is.null(vt)
    bounds <- c(prob_min, 1-prob_min)
    NC <- length(parm)
    ind_pp <- pp:NC

    #*** iterations
    while (iterate){
        parm_old <- parm
        probs0 <- reglca_calc_probs(parm=parm)
        # evaluate log-likelihood
        q0 <- reglca_freq_ll( x=probs0, C=C, W=W )
        # 1st derivative
        contr <- C / probs0 - W / (1-probs0)
        f1 <- sum(contr[ind_pp])
        # 2nd derivative
        contr <- -C / probs0^2 - W / (1-probs0)^2
        f2 <- sum(contr[ind_pp])
        # parameter update
        incr <- - sign(f2) * f1 / ( abs(f2) + eps )
        incr <- cdm_trim_increment( increment=incr, max.increment=max_increment, type=1)
        max_increment <- min( .10, max( abs(incr) ) / increment_factor )
        parm[pp] <- parm[pp] + incr
        parm[pp] <- cdm_squeeze( x=parm[pp], bounds=bounds )

        #-- apply threshold operator
        if (pp>1){
            if ( vt_null ){
                vt <- abs(f2) + eps
            }
            parm[pp] <- reglca_threshold_parameter(x=parm[pp], regular_type=regular_type,
                                lambda=lambda, vt=vt)
        }
        iter <- iter + 1
        if ( iter > cd_steps ){ iterate <- FALSE }
        parchange <- abs( parm[pp] - parm_old[pp] )
        if ( parchange < conv ){ iterate <- FALSE }
    }
    #-- output
    return(parm)
}
