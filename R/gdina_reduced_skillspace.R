## File Name: gdina_reduced_skillspace.R
## File Version: 0.17
###################################################
# auxiliary function reduced skill space
gdina_reduced_skillspace <- function( ntheta, Z,
    reduced.skillspace.method=2, eps=1E-10 )
{
    #***********************************
    ntheta <- cdm_sumnorm( ntheta )
    lntheta <- matrix(log(ntheta+eps),ncol=1 )
    V <- diag( ntheta)
    #---------------------------
    #*** skill space method 1 (CDM <=2.5)
    if ( reduced.skillspace.method==1){
        Z1 <- crossprod(Z, V) %*% Z
        diag(Z1) <- diag(Z1)+eps
        covbeta <- solve(Z1)
        beta <- covbeta %*% ( crossprod(Z,V) %*% lntheta )
    }
    #------------------------------
    #*** skill space method 2 (CDM >=2.6)
    if ( reduced.skillspace.method==2){
        mod <- stats::lm( lntheta ~ 0 + Z, weights=ntheta )
        beta <- matrix( mod$coef, nrow=ncol(Z), ncol=1 )
        beta[ is.na(beta) ] <- 0
    }
    #*******************************************
    # calculate attribute probability
    attr.prob <- reduced_skillspace_beta_2_probs( Z=Z, beta=beta )
    #***** output
    res <- list("beta"=beta, "attr.prob"=attr.prob)
    return(res)
}
############################################################

gdina.reduced.skillspace <- gdina_reduced_skillspace
