## File Name: slca_est_Xlambda.R
## File Version: 0.30


###########################################################################
# estimation of Xlambda parameters
slca_est_Xlambda <- function(Xlambda, Xdes, probs, n.ik1, N.ik1, I, K, G,
    max.increment, TP,msteps, convM, Xlambda.fixed, XdesM, dimXdes, oldfac,
    decrease.increments, dampening_factor=1.01, Xlambda.constr.V, e2, V1,
    regularization, regular_lam_used, regular_n, Xlambda_positive, regular_type )
{
    max.increment0 <- max.increment
    iter <- 1
    eps <- 1e-8
    parchange <- 1
    Xlambda00 <- Xlambda
    Nlam <- length(Xlambda)
    n.ik <- aperm( n.ik1, c(2,3,1) )
    N.ik <- aperm( N.ik1, c(2,1) )
    maxK <- K+1
    #--------------------------------
    # begin M-steps
    while( ( iter <=msteps ) & ( parchange > convM)  ){
        Xlambda0 <- Xlambda
        probs <- slca_calc_prob( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda )
        d2.b <- d1.b <- rep(eps,Nlam)
        # probs  num [1:I, 1:maxK, 1:TP]
        # n.ik  num [1:I, 1:maxK, 1:TP]
        # N.ik  num [1:I,1:TP]
        # Xdes  num [1:I, 1:maxK, 1:TP, 1:Nlam]
        #-- calculate derivatives
        res <- slca_est_Xlambda_calc_deriv( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda,
                        probs=probs, n.ik=n.ik, N.ik=N.ik )
        d1.b <- res$d1b
        d2.b <- res$d2b
        #-- calculate increment
        res <- slca_est_Xlambda_calc_increment( d1=d1.b, d2=d2.b, x0=Xlambda, regularization=regularization,
                        regular_lam_used=regular_lam_used, max.increment=max.increment, regular_type=regular_type )
        increment <- res$increment
        max.increment <- res$max.increment

        #-- update parameter
        Xlambda <- Xlambda + increment
        se.Xlambda <- sqrt( 1 / abs( d2.b+ eps ) )
        #-- positivity constraint
        Xlambda <- cdm_positivity_restriction(x=Xlambda, positive=Xlambda_positive)
        #-- parameter fixings
        res <- cdm_include_fixed_parameters( parm=Xlambda, se_parm=se.Xlambda, parm_fixed=Xlambda.fixed )
        Xlambda <- res$parm
        se.Xlambda <- res$se_parm

        iter <- iter + 1
        parchange <- max( abs(Xlambda0-Xlambda))
    }  # end M-steps
    #-----------------------------------------

        # linear constraints on Xlambda parameters
        # below is code copied from rasch.pml3 (sirt package)
        #................
            # linear constraints: Let e be the vector of error
            # correlations, V a design matrix and c a vector.
            # The constraints can be written in the form
            # c=V * e . Then V*e - c=0.
            # See the Neuhaus paper:
            # e_cons=e + V * (V'V)^(-1) * ( c - V * e )
    if ( ! is.null(Xlambda.constr.V) ){
        Xlambda <- slca_est_xlambda_constraint( Xlambda.constr.V=Xlambda.constr.V, V1=V1, e2=e2 )
    }

    if (oldfac > 0 ){
        Xlambda <- oldfac*Xlambda00 + ( 1 - oldfac ) *Xlambda
    }
    max.increment <- max( abs( Xlambda - Xlambda00 ))
    if (decrease.increments){
        max.increment0 <- max.increment0 / dampening_factor
    }
    penalty <- cdm_penalty_values(x=Xlambda, regular_type=regular_type, regular_lam=regular_lam_used)
    regular_penalty <- regular_n * sum( penalty )
    #----- output
    res <- list(Xlambda=Xlambda, se.Xlambda=se.Xlambda, max.increment=max.increment0, regular_penalty=regular_penalty)
    return(res)
}


.slca.est.Xlambda <- slca_est_Xlambda
