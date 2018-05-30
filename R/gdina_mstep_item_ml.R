## File Name: gdina_mstep_item_ml.R
## File Version: 0.863

#####################################################
# GDINA M-step item parameters
gdina_mstep_item_ml <- function( pjjj, Ilj.ast, Rlj.ast, eps, avoid.zeroprobs,
        Mjjj, invM.list, linkfct, rule, method, iter, delta.new, max.increment, fac.oldxsi,
        jj, delta, rrum.model, delta.fixed, mstep_iter, mstep_conv, devchange,
        regular_type, regular_lam, cd_steps, mono.constr, Aj_mono_constraints_jj, mono_maxiter,
        regular_alpha, regular_tau, regularization_types, prior_intercepts, prior_slopes, use_prior,
        use_optim=FALSE )
{
    eps2 <- eps
    delta_jj <- delta[[jj]]
    NP <- length(delta_jj)
    converged <- FALSE
    penalty <- 0
    logprior_value <- 0
    ii <- 0
    h <- 1E-4   # numerical differentiation parameter

    max_increment <- max.increment

    eps_squeeze <- 1E-6

    Rlj.ast <- Rlj.ast + .005
    Ilj.ast <- Ilj.ast + .05
    N <- sum(Ilj.ast)

    diag_only <- FALSE
    regularization <- FALSE

    if ( regular_type %in% regularization_types ){
        diag_only <- TRUE
        regularization <- TRUE
        max_increment <- max( 3*regular_lam, max_increment )
    }

    #---- prior function
    logprior_FUN <- function(x, p1, p2)
    {
        lp1 <- log_dgnorm(x=x[1], loc=p1[1], scale=p1[2], power=p1[3] )
        lp2 <- log_dgnorm(x=x[-1], loc=p2[1], scale=p2[2], power=p2[3] )
        lp <- sum(lp1) + sum(lp2)
        return(lp)
    }
    #----

    #------------------ define log-likelihood function
    ll_FUN <- function(x)
                {
                    irf1 <- gdina_prob_item_designmatrix( delta_jj=x, Mjjj=Mjjj, linkfct=linkfct, eps_squeeze=eps )
                    ll <- - sum( Rlj.ast * log(abs(irf1)) + ( Ilj.ast - Rlj.ast ) * log( abs(1 - irf1 ) ) )
                    if (use_prior){
                        ll <- ll - logprior_FUN(x=x, p1=prior_intercepts, p2=prior_slopes)
                    }
                    return(ll)
                }
    #------------------

    #------------------ define log-likelihood function
    ll_FUN0 <- function(x)
                {
                    irf1 <- gdina_prob_item_designmatrix( delta_jj=x, Mjjj=Mjjj, linkfct=linkfct, eps_squeeze=eps )
                    ll <- - sum( Rlj.ast * log(abs(irf1)) + ( Ilj.ast - Rlj.ast ) * log( abs(1 - irf1 ) ) )
                    return(ll)
                }
    #------------------
    #--- algorithm without monotonicity constraints
    if ( ! use_optim){
    delta_jj <- gdina_mstep_item_ml_algorithm( delta_jj=delta_jj, max_increment=max_increment, regular_lam=regular_lam,
                        regular_type=regular_type, regularization=regularization, ll_FUN=ll_FUN, h=h,
                        mstep_conv=mstep_conv, cd_steps=cd_steps, mstep_iter=mstep_iter,
                        regular_alpha=regular_alpha, regular_tau=regular_tau )
    }                    
    if (use_optim){
        mod <- stats::optim(par=delta_jj, fn=ll_FUN, method="L-BFGS-B")
        delta_jj <- mod$par
    }                        
                        
    ll_value <- ll_FUN(x=delta_jj)

    #--- algorithm with monotonicity constraints
    if (mono.constr){

        C <- C0 <- abs(ll_FUN(delta_jj))
        iterate_mono <- FALSE
        crit_pen <- 1E-10
        delta_jj_uncon <- delta_jj
        irf1 <- gdina_prob_item_designmatrix( delta_jj=delta_jj, Mjjj=Mjjj, linkfct=linkfct, eps_squeeze=eps )
        constraints_fitted_jj <- as.vector( Aj_mono_constraints_jj %*% irf1 )
        penalty_constraints <- gdina_mstep_mono_constraints_penalty(x=constraints_fitted_jj)
        if ( sum(penalty_constraints) > crit_pen ){
            iterate_mono <- TRUE
        }

        #------------------ define log-likelihood function with penalty
        ll_FUN_mono <- function(x)
                    {
                        irf1 <- gdina_prob_item_designmatrix( delta_jj=x, Mjjj=Mjjj, linkfct=linkfct, eps_squeeze=eps )
                        ll <- - sum( Rlj.ast * log(abs(irf1)) + ( Ilj.ast - Rlj.ast ) * log( abs(1 - irf1 ) ) )
                        if (use_prior){
                            ll <- ll - logprior_FUN(x=x, p1=prior_intercepts, p2=prior_slopes)
                        }
                        # constraints
                        y <- gdina_mstep_mono_constraints_penalty( as.vector( Aj_mono_constraints_jj %*% irf1 ) )
                        y <- C * sum( y^2 )
                        ll <- ll + y
                        return(ll)
                    }
        #------------------

        hh <- 1
        while (iterate_mono){

            #------------------ define log-likelihood function with penalty
            ll_FUN_mono <- function(x)
                        {
                            irf1 <- gdina_prob_item_designmatrix( delta_jj=x, Mjjj=Mjjj, linkfct=linkfct, eps_squeeze=eps )
                            ll <- - sum( Rlj.ast * log(abs(irf1)) + ( Ilj.ast - Rlj.ast ) * log( abs(1 - irf1 ) ) )
                            if (use_prior){
                                ll <- ll - logprior_FUN(x=x, p1=prior_intercepts, p2=prior_slopes)
                            }
                            # constraints
                            y <- gdina_mstep_mono_constraints_penalty( as.vector( Aj_mono_constraints_jj %*% irf1 ) )
                            y <- C * sum( y^2 )
                            ll <- ll + y
                            return(ll)
                        }
            #------------------

            delta_jj <- gdina_mstep_item_ml_algorithm( delta_jj=delta_jj, max_increment=max_increment,
                                regular_lam=regular_lam, regular_type=regular_type,
                                regularization=regularization, ll_FUN=ll_FUN_mono, h=h,
                                mstep_conv=mstep_conv, cd_steps=cd_steps, mstep_iter=mstep_iter )
            irf1 <- gdina_prob_item_designmatrix( delta_jj=delta_jj, Mjjj=Mjjj, linkfct=linkfct, eps_squeeze=eps )
            constraints_fitted_jj <- as.vector( Aj_mono_constraints_jj %*% irf1 )
            penalty_constraints <- gdina_mstep_mono_constraints_penalty(x=constraints_fitted_jj)
            if ( sum(penalty_constraints) < crit_pen ){
                iterate_mono <- FALSE
            }
            C <- 10*C
            hh <- hh+1
            if (hh > mono_maxiter){
                iterate_mono <- FALSE
            }

        } # end iterations for monotonicity constraints
        ll_value <- ll_FUN_mono(x=delta_jj)
    }

    delta.new[[jj]] <- delta_jj
    if ( (fac.oldxsi > 0 ) & (iter>3) ){
        fac.oldxsi1 <- fac.oldxsi * ( devchange >=0 )
        delta.new[[jj]] <- fac.oldxsi1*delta[[jj]] + ( 1 - fac.oldxsi1 ) * delta.new[[jj]]
    }

    # fix delta parameter here!!
    if ( ! is.null( delta.fixed ) ){
        delta.fixed.jj <- delta.fixed[[jj]]
        if ( ! is.na( delta.fixed.jj)[1] ){
                delta.new[[jj]] <- delta.fixed.jj
        }
    }

    #-- penalty parameter for item
    delta_jj <- delta.new[[jj]]
    if (regularization){
        x <- delta_jj[-1]
        penalty1 <- cdm_penalty_values( x=x, regular_type=regular_type, regular_lam=regular_lam,
                            regular_alpha=regular_alpha, regular_tau=regular_tau )
        penalty <- N*sum(penalty1)
        ll_value <- N*ll_value - penalty
    }
    if (use_prior){
        logprior_value <- logprior_FUN(x=delta_jj, p1=prior_intercepts, p2=prior_slopes)
    }

    #*** output
    res <- list( delta.new=delta.new, penalty=penalty, ll_value=ll_value,
                        logprior_value=logprior_value)
    return(res)
}
######################################################

