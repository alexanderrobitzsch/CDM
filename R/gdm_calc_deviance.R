## File Name: gdm_calc_deviance.R
## File Version: 0.06

gdm_calc_deviance <- function(G, use.freqpatt, ind.group, p.xi.aj, pi.k, weights)
{
    # n.ik [ TP, I, K+1, G ]
    # N.ik [ TP, I, G ]
    # probs [I, K+1, TP ]
    ll <- 0
    for (gg in 1:G){
        if ( ! use.freqpatt ){
            ind.gg <- ind.group[[gg]]
            ll <- ll + sum( weights[ind.gg] * log( rowSums( p.xi.aj[ind.gg,] *
                            matrix( pi.k[,gg], nrow=length(ind.gg), ncol=nrow(pi.k), byrow=TRUE ) ) ) )
        }
        if ( use.freqpatt ){
            if (G>1){
                wgg <- weights[,gg]
            }
            if (G==1){
                wgg <- weights
            }
            ll <- ll + sum( wgg * log( rowSums( p.xi.aj * matrix( pi.k[,gg], nrow=nrow(p.xi.aj),
                                    ncol=nrow(pi.k), byrow=TRUE ) ) ) )
        }
    }
    dev <- -2*ll
    #--- OUTPUT
    res <- list( ll=ll, dev=dev)
    return(res)
}
