## File Name: reglca_calc_deviance.R
## File Version: 0.09

reglca_calc_deviance <- function( p.xi.aj, class_probs, weights, loglike,
            penalty=0, opt_fct=0, ind_groups, G, N_groups )
{
    eps <- 1E-30
    p.xi.aj[ p.xi.aj > 1 ] <- 1 - eps
    p.xi.aj[ p.xi.aj < 0 ] <- eps
    N <- nrow(p.xi.aj)

    if (G==1){
        class_probs_mat <- cdm_matrix2( class_probs, nrow=N )
    } else {
        class_probs_mat <- matrix(NA, nrow=N, ncol=nrow(class_probs) )
        for (gg in 1:G){
            ind_gg <- ind_groups[[gg]]
            class_probs_mat[ ind_gg, ] <- cdm_matrix2( class_probs[,gg], nrow=N_groups[gg] )
        }
    }

    l1 <- rowSums( p.xi.aj * class_probs_mat ) + eps
    l1[ l1 < 0 ] <- eps

    like.new <- sum( log( l1 ) * weights)
    likediff <- abs( loglike - like.new )

    #--- regularization
    opt_fct_old <- opt_fct
    opt_fct <- -2*like.new + 2* penalty
    opt_fct_change <- - opt_fct + opt_fct_old

    #--- OUTPUT
    res <- list( like.new=like.new, likediff=likediff, opt_fct=opt_fct,
                    opt_fct_change=opt_fct_change)
    return(res)
}
