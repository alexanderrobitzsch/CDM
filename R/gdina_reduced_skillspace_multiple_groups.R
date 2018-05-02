## File Name: gdina_reduced_skillspace_multiple_groups.R
## File Version: 0.10

gdina_reduced_skillspace_multiple_groups <- function( Z, reduced.skillspace.method, item_patt_freq_matr,
            p.aj.xi, G )
{
    #---- single group
    if (G==1){
        res <- gdina_reduced_skillspace_single_group( Z=Z, reduced.skillspace.method=reduced.skillspace.method,
                            ipmat=item_patt_freq_matr, post=p.aj.xi )
    }
    #---- multiple groups
    if (G>1){
        NZ <- ncol(Z)
        L <- nrow(Z)
        beta <- matrix(NA, nrow=NZ, ncol=G)
        attr.prob <- matrix(NA, nrow=L, ncol=G)
        for (gg in 1:G){
            ipmat <- item_patt_freq_matr[,,gg]
            post <- p.aj.xi[,,gg]
            res <- gdina_reduced_skillspace_single_group( Z=Z, reduced.skillspace.method=reduced.skillspace.method,
                                ipmat=ipmat, post=post )
            beta[,gg] <- res$beta
            attr.prob[,gg] <- res$attr.prob
        }
        res <- list(beta=beta, attr.prob=attr.prob)
    }
    #--- output
    return(res)
}
