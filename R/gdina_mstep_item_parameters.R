## File Name: gdina_mstep_item_parameters.R
## File Version: 0.671

gdina_mstep_item_parameters <- function(R.lj, I.lj, aggr.patt.designmatrix, max.increment,
        increment.factor, J, Aj, Mj, delta, method, avoid.zeroprobs, invM.list, linkfct,
        rule, iter, fac.oldxsi, rrum.model, delta.fixed, devchange, mstep_iter, mstep_conv,
        Mj.index, suffstat_probs, regular_lam, regular_type, cd_steps,
        mono.constr, Aj_mono_constraints, mono_maxiter, regular_alpha, regular_tau,
        regularization_types, prior_intercepts, prior_slopes, use_prior,
        optimizer="CDM", regularization=FALSE, regular_weights=NULL )
{
    mono_constraints_fitted <- NULL
    # calculation of expected counts
    R.ljM <- R.lj %*% aggr.patt.designmatrix
    I.ljM <- I.lj %*% aggr.patt.designmatrix
    penalty <- 0
    ll_value <- 0
    logprior_value <- 0

    eps2 <- eps <- 1E-10
    max.increment <- max.increment / increment.factor
    delta.new <- NULL

    #----- loop over items
    for (jj in 1:J){     # begin item

        Ajjj <- Aj[[jj]]
        Mjjj <- Mj[[jj]][[1]]
        Rlj.ast <- R.ljM[ jj, Mj.index[jj,5]:Mj.index[jj,6] ]
        Ilj.ast <- I.ljM[ jj, Mj.index[jj,5]:Mj.index[jj,6] ]
        pjjj <- Rlj.ast / ( Ilj.ast + eps2 )
        suffstat_probs[[jj]] <- pjjj
        #--- define argument list
        if ( method %in% c("ULS","WLS","ML") ){
            arglist <- list( pjjj=pjjj, Ilj.ast=Ilj.ast, Rlj.ast=Rlj.ast, eps=eps,
                            avoid.zeroprobs=avoid.zeroprobs, Mjjj=Mjjj, invM.list=invM.list,
                            linkfct=linkfct, rule=rule,    method=method, iter=iter,
                            delta.new=delta.new, max.increment=max.increment, fac.oldxsi=fac.oldxsi,
                            jj=jj, delta=delta, rrum.model=rrum.model, delta.fixed=delta.fixed,
                            devchange=devchange )
        }
        #*** optimization ULS / WLS
        if ( method %in% c("ULS","WLS") ){
            res_jj <- do.call( what=gdina_mstep_item_uls, args=arglist )
        }
        #*** optimization ML
        rrum <- ( rule[jj]=="ACDM" )    & ( linkfct=="log")
        if ( method %in% c("ML") ){
            arglist$mstep_iter <- mstep_iter
            arglist$mstep_conv <- mstep_conv
            if ( ! rrum ){
                arglist$regular_lam <- regular_lam
                arglist$regular_type <- regular_type
                arglist$cd_steps <- cd_steps
                arglist$mono.constr <- mono.constr
                arglist$Aj_mono_constraints_jj <- Aj_mono_constraints[[jj]]
                arglist$mono_maxiter <- mono_maxiter
                arglist$regular_alpha <- regular_alpha
                arglist$regular_tau <- regular_tau
                arglist$regularization_types <- regularization_types
                arglist$prior_intercepts <- prior_intercepts
                arglist$prior_slopes <- prior_slopes
                arglist$use_prior <- use_prior
                arglist$optimizer <- optimizer
                arglist$regular_weights <- regular_weights
                #-- estimation step
                res_jj <- do.call( what=gdina_mstep_item_ml, args=arglist )
                penalty <- penalty + res_jj$penalty
                ll_value <- ll_value + res_jj$ll_value
                logprior_value <- logprior_value + res_jj$logprior_value
            }
            if ( rrum ){
                arglist$optimizer <- optimizer
                res_jj <- do.call( what=gdina_mstep_item_ml_rrum, args=arglist )
            }
        }
        delta.new <- res_jj$delta.new

    }        # end item

    # number of regularized item parameters
    res <- gdina_mstep_item_parameters_number_of_regularized_parameters(
                        regularization=regularization, delta=delta, J=J)
    numb_regular_pars <- res$numb_regular_pars
    delta_regularized <- res$delta_regularized

    #----------------- OUTPUT -------------
    res <- list( delta.new=delta.new, suffstat_probs=suffstat_probs,
                    mono_constraints_fitted=mono_constraints_fitted,
                    penalty=penalty, ll_value=ll_value, logprior_value=logprior_value,
                    numb_regular_pars=numb_regular_pars, delta_regularized=delta_regularized)
    return(res)
}
