## File Name: gdm_est_skillspace.R
## File Version: 0.09



###########################################################################
# reduced skillspace estimation
gdm_est_skillspace <- function(Ngroup, pi.k, Z, G, delta, eps=1E-10, estimate=TRUE )
{
    covdelta <- as.list(1:G)
    covbeta <- NULL
    for (gg in 1:G){
        if (estimate){
            ntheta <- cdm_sumnorm( Ngroup[gg] * pi.k[,gg] )
            lntheta <- log(ntheta+eps)
            mod <- stats::lm( lntheta ~ 0 + Z, weights=ntheta )
            covbeta <- vcov(mod)
            beta <- coef(mod)
            delta[,gg] <- beta
        }
        pi.k[,gg] <- cdm_sumnorm( exp( Z %*% delta[,gg] ) / Ngroup[gg] )
        covdelta[[gg]] <- covbeta
    }
    #--- OUTPUT
    res <- list( pi.k=pi.k, delta=delta, covdelta=covdelta )
    return(res)
}


.gdm.est.skillspace <- gdm_est_skillspace
