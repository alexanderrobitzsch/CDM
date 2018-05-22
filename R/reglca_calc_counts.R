## File Name: reglca_calc_counts.R
## File Version: 0.09


#----- calculation of expected counts
reglca_calc_counts <- function(weights, dat, dat.resp, p.aj.xi, K, n.ik, TP, I, dat.ind2,
        ind_groups, G )
{
    #--- single group
    if (G==1){
        N.ik <- matrix( 0, nrow=TP, ncol=I)
        n.ik <- array( 0, dim=c(TP,I,K+1) )
        for (kk in 1:(K+1)){
            dkk2 <- dat.ind2[[kk]]
            n.ik[,,kk] <- crossprod( p.aj.xi, dkk2 )
            N.ik <- N.ik + n.ik[,,kk]
        }
    }
    #--- multiple groups
    if (G>1){
        N.ik <- array( 0, dim=c(TP, I, G) )
        n.ik <- array( 0, dim=c(TP,I,K+1, G) )
        for (gg in 1:G){
            ind_gg <- ind_groups[[gg]]
            for (kk in 1:(K+1)){
                dkk2 <- dat.ind2[[kk]]
                n.ik[,,kk,gg] <- crossprod( p.aj.xi[ind_gg,], dkk2[ ind_gg, ] )
                N.ik[,,gg] <- N.ik[,,gg] + n.ik[,,kk,gg]
            }
        }
    }

    #----- output
    res <- list(n.ik=n.ik, N.ik=N.ik)
    return(res)
}

