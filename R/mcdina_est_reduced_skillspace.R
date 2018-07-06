## File Name: mcdina_est_reduced_skillspace.R
## File Version: 0.03

mcdina_est_reduced_skillspace <- function(pi.k, Z)
{
    G <- ncol(pi.k)
    for (gg in 1:G){
        ntheta <- pi.k[,gg]
        res <- gdina_reduced_skillspace( ntheta=ntheta, Z=Z,
                        reduced.skillspace.method=2 )
        pi.k[,gg] <- res$attr.prob
    }
    return(pi.k)
}
