## File Name: slca_calc_loglikelihood.R
## File Version: 0.09

slca_calc_loglikelihood <- function(Xlambda, delta, delta.designmatrix, XdesM, dimXdes, gwt0, dat, I, resp.ind.list,
        G, use.freqpatt, ind.group, weights, Xlambda.constr.V, e2, V1, Xlambda_positive)
{
    #-- Xlambda constraint
    if ( ! is.null(Xlambda.constr.V) ){
        Xlambda <- slca_est_xlambda_constraint( Xlambda.constr.V=Xlambda.constr.V, V1=V1, e2=e2 )
    }
    #-- positivity constraint
    Xlambda <- cdm_positivity_restriction(x=Xlambda, positive=Xlambda_positive)
    #-- compute log-likelihood
    probs <- slca_calc_prob( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda )
    pi.k <- slca_calc_class_probabilities( delta=delta, delta.designmatrix=delta.designmatrix )
    res <- slca_calc_posterior( probs=probs, gwt0=gwt0, dat=dat, I=I, resp.ind.list=resp.ind.list )
    ll <- slca_calc_likelihood( G=G, use.freqpatt=use.freqpatt, ind.group=ind.group, p.xi.aj=res$hwt,
                pi.k=pi.k, weights=weights )
    #--- output
    res <- list(ll=ll, pi.k=pi.k, Xlambda=Xlambda, delta=delta)
    return(res)
}
