## File Name: gdina_calc_loglikelihood.R
## File Version: 0.05

gdina_calc_loglikelihood <- function(delta_vec, beta, attr.prob, Z, delta_indices, J,
        iter, disp, L, aggr.attr.patt, Mj, linkfct, IP, item.patt.split,
        resp.ind.list, zeroprob.skillclasses, item.patt.freq, loglike, G, reduced.skillspace )
{

    # input: delta, attr.prob, beta

    # reconvert delta
    delta <- delta.new <- gdina_delta_convert_into_list( delta_vec=delta_vec, delta_indices=delta_indices, J=J)
    # beta parameter
    if ( ! is.null(beta) ){
        if (G==1){
            attr.prob <- reduced_skillspace_beta_2_probs( Z=Z, beta=beta )
        } else {
            attr.prob <- matrix(NA, nrow=nrow(Z), ncol=G)
            for (gg in 1:G){
                attr.prob[,gg] <- reduced_skillspace_beta_2_probs( Z=Z, beta=beta[,gg] )
            }
        }
    } else {
        if (G==1){
            attr.prob <- cdm_shrink_positive(x=attr.prob)
        } else {
            for (gg in 1:G){
                attr.prob[,gg] <- cdm_shrink_positive(x=attr.prob[,gg])
            }
        }

    }
    #-- calculate total log-likelihood
    pjM <- gdina_calc_prob( progress=FALSE, iter=iter, disp=disp, J=J, L=L,
                    aggr.attr.patt=aggr.attr.patt, Mj=Mj, delta=delta, linkfct=linkfct )
    p.xi.aj <- gdina_calc_individual_likelihood( IP=IP, L=L, pjM=pjM, item.patt.split=item.patt.split,
                        J=J, resp.ind.list=resp.ind.list, zeroprob.skillclasses=zeroprob.skillclasses )
    ll <- gdina_calc_deviance( p.xi.aj=p.xi.aj, attr.prob=attr.prob, item.patt.freq=item.patt.freq,
                    loglike=loglike, G=G, IP=IP )$like.new

    res <- list( ll=ll, attr.prob=attr.prob, delta.new=delta.new, beta=beta, delta_vec=delta_vec )
    return(res)
}
