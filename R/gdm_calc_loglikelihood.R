## File Name: gdm_calc_loglikelihood.R
## File Version: 0.07

gdm_calc_loglikelihood <- function(irtmodel, skillspace, b, a, centerintercepts, centerslopes, TD, Qmatrix,
        Ngroup, pi.k, delta.designmatrix, delta, G, theta.k, D, mean.constraint, Sigma.constraint,
        standardized.latent, p.aj.xi, group, ind.group, weights, thetaDes, I, K, gwt0, dat,
        resp.ind.list, use.freqpatt, p.xi.aj, TP )
{
    #---------------------------------
    # constraints on parameters
    b <- gdm_est_b_centerintercepts( b=b, centerintercepts=centerintercepts, TD=TD, Qmatrix=Qmatrix )
    if (irtmodel=="2PL"){
        a <- gdm_est_a_centerslopes( a=a, centerslopes=centerslopes, Qmatrix=Qmatrix, TD=TD )
    }

    #----------------------------------
    #--- constraints on skill space
    if ( skillspace == "loglinear" ){
        res <- gdm_est_skillspace( Ngroup=Ngroup, pi.k=pi.k, Z=delta.designmatrix, G=G, delta=delta, estimate=FALSE )
        pi.k <- res$pi.k
        delta <- res$delta
    }
    if ( skillspace == "normal" ){
        res <- gdm_est_normalskills( pi.k=pi.k, theta.k=theta.k, irtmodel=irtmodel, G=G, D=D,
                        mean.constraint=mean.constraint, Sigma.constraint=Sigma.constraint,
                        standardized.latent=standardized.latent, p.aj.xi=p.aj.xi, group=group, ind.group=ind.group,
                        weights=weights, b=b, a=a )
        pi.k <- res$pi.k
        b <- res$b
        a <- res$a
    }
    if ( skillspace == "est" ){
        thetaDes <- theta.k
    }
    #--- probabilities
    probs <- gdm_calc_prob( a=a, b=b, thetaDes=thetaDes, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD )
    #--- posterior
    res.hwt <- gdm_calc_posterior( probs=probs, gwt0=gwt0, dat=dat, I=I, resp.ind.list=resp.ind.list )
    p.xi.aj <- res.hwt$hwt
    res <- gdm_calc_deviance( G=G, use.freqpatt=use.freqpatt, ind.group=ind.group, p.xi.aj=p.xi.aj,
                    pi.k=pi.k, weights=weights )
    ll <- res$ll
    #--- output
    res <- list(ll=ll, pi.k=pi.k, theta.k=theta.k, thetaDes=thetaDes, a=a, b=b, delta=delta)
    return(res)
}
