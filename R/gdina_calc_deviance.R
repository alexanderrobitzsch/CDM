## File Name: gdina_calc_deviance.R
## File Version: 0.16

gdina_calc_deviance <- function( p.xi.aj , attr.prob, item.patt.freq, loglike, G, IP,
        regularization, penalty=0, opt_fct=0, logprior_value=0 )
{
    eps <- 1E-30
    # calculate the updated likelihood
    p.xi.aj[ p.xi.aj > 1 ] <- 1 - eps
    p.xi.aj[ p.xi.aj < 0 ] <- eps
    if (G==1){
        l1 <- rowSums( p.xi.aj * cdm_matrix2( attr.prob , nrow=IP ) ) + eps
        l1[ l1 < 0 ] <- eps
    }
    if (G>1){
        l1 <- matrix( 0 , IP , G )
        for (gg in 1:G){
            l1[,gg] <- rowSums( p.xi.aj * cdm_matrix2( attr.prob[,gg] , nrow=IP ) ) + eps
            l1[ l1[,gg] < 0 ,gg] <- eps
        }
    }
    like.new <- sum( log( l1 ) * item.patt.freq )
    likediff <- abs( loglike - like.new )

    #--- regularization
    opt_fct_old <- opt_fct
    opt_fct <- -2*like.new + 2* penalty - 2*logprior_value
    opt_fct_change <- - opt_fct + opt_fct_old

    #--- OUTPUT
    res <- list( like.new=like.new, likediff=likediff, opt_fct = opt_fct,
                    opt_fct_change=opt_fct_change)
    return(res)
}
