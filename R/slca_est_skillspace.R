## File Name: slca_est_skillspace.R
## File Version: 0.06

###########################################################################
# reduced skillspace estimation
slca_est_skillspace <- function(Ngroup, pi.k , delta.designmatrix , G , delta , delta.fixed ,
            eps=1E-10 , oldfac , delta.linkfct)
{
    covdelta <- as.list(1:G)
    Z <- delta.designmatrix
    delta0 <- delta
    ND <- length(delta)
    for (gg in 1:G){
        if ( delta.linkfct == "log"){
            ntheta <- cdm_sumnorm( Ngroup[gg] * pi.k[,gg] )
            pij <- log(ntheta+eps)
            mod <- stats::lm( pij ~ 0 + Z , weights = ntheta )
            covbeta <- vcov(mod)
            beta <- coef(mod)
        }
        if ( delta.linkfct == "logit"){
            nj <- Ngroup[gg] * pi.k[,gg]
            pij <- stats::qlogis( pi.k[,gg] + eps )
            wj <- cdm_sumnorm( stats::plogis( delta.designmatrix %*% delta[,gg,drop=FALSE] ) )
            wj <- wj[,1]
            n <- Ngroup[gg]
            mod1 <- stats::lm( pij ~ 0 + Z )
            beta <- coef(mod1)
            covbeta <- vcov(mod1)
        }
        if ( ! is.null( delta.fixed ) ){
            # delta.fixed: 1st column: parameter index
            #              2nd column: group index
            #              3rd column: parameter value
            ind.gg <- which( delta.fixed[ ,2] == gg )
            if ( length(ind.gg) > 0 ){
                beta[ delta.fixed[ind.gg,1] ] <- delta.fixed[ind.gg,3]
            }
        }
        if ( delta.linkfct == "log"){
            pi.k[,gg] <- cdm_sumnorm( exp( Z %*% beta ) / Ngroup[gg] )
        }
        if ( delta.linkfct == "logit"){
            pi.k[,gg] <- cdm_sumnorm( exp( delta.designmatrix %*% beta ) )
        }
        if ( oldfac > 0 ){
            beta <- oldfac*delta0[,gg] + ( 1 - oldfac)*beta
        }
        delta[,gg] <- beta
        covdelta[[gg]] <- covbeta
    }
    res <- list( pi.k=pi.k , delta=delta ,     covdelta = covdelta )
    return(res)
}

.slca.est.skillspace <- slca_est_skillspace

