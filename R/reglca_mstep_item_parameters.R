## File Name: reglca_mstep_item_parameters.R
## File Version: 0.482


reglca_mstep_item_parameters <- function(I, n.ik, N.ik, h, mstep_iter, conv, regular_lam,
        regular_type, cd_steps, item_probs, max_increment, iter, G, fac=1.02,
        prob_min=0)
{
    penalty <- rep(0,I)
    n_par <- rep(0,I)
    n_reg <- 0
    n_reg_item <- rep(0,I)
    nclasses <- ncol(item_probs)
    expected_loglike <- rep(0,I)
    opt_fct_item_sum <- rep(0,I)
    bounds <- c( prob_min, 1-prob_min)
    
    #-- sum counts in case of multiple groups
    if (G>1){
        ND <- dim(n.ik)
        n.ik0 <- n.ik
        N.ik0 <- N.ik
        n.ik <- array( 0, dim=ND[1:3] )
        N.ik <- matrix( 0, nrow=ND[1], ncol=ND[2] )
        for (gg in 1:G){
            n.ik <- n.ik + n.ik0[,,,gg]
            N.ik <- N.ik + N.ik0[,,gg]
        }
    }

    item_probs0 <- item_probs
    for (ii in 1:I){
        freq <- n.ik[,ii,2] / N.ik[,ii]
        res <- reglca_fit_probabilities( freq=freq, h=h, maxit=mstep_iter, conv=conv, verbose=FALSE,
                            parm_init=NULL, lambda=regular_lam, regular_type=regular_type,
                            cd_steps=cd_steps, max_increment=max_increment, prob_min=prob_min)
        expected_loglike[ii] <- res$ll
        opt_fct_item_sum[ii] <- res$ll + res$pen
        incr <- res$probs - item_probs[ii,]
        incr <- cdm_trim_increment( increment=incr, max.increment=max_increment, type=1 )
        item_probs[ii,] <- item_probs[ii,] + incr
        item_probs[ii,] <- cdm_squeeze( x=item_probs[ii,], bounds=bounds)
        penalty[ii] <- sum(N.ik[,ii]) * res$pen
        n_par[ii] <- res$n_par
        n_reg_item[ii] <- nclasses - res$n_par
        n_reg <- n_reg + nclasses - res$n_par
    }
    penalty <- sum(penalty)
    n_par <- sum(n_par)
    opt_fct_item_sum <- sum(opt_fct_item_sum)
    max_increment <- min( max_increment, max( abs( item_probs - item_probs0 ) )     ) / fac
    #--- output
    res <- list( item_probs=item_probs, penalty=penalty, n_par=n_par, n_reg=n_reg, max_increment=max_increment,
                        n_reg_item=n_reg_item, opt_fct_item_sum=opt_fct_item_sum)
    return(res)
}

