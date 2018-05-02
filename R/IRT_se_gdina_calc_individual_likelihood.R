## File Name: IRT_se_gdina_calc_individual_likelihood.R
## File Version: 0.16

IRT_se_gdina_calc_individual_likelihood <- function(J, L, aggr.attr.patt, Mj, delta, linkfct,
        IP, item.patt.split, resp.ind.list, zeroprob.skillclasses, G, item.patt.freq,
        Z.skillspace, beta, reduced.skillspace, return_all = FALSE )
{
    pjM <- gdina_calc_prob( progress=FALSE, iter=100, disp="", J=J, L=L,
                    aggr.attr.patt=aggr.attr.patt, Mj=Mj, delta=delta, linkfct=linkfct )
    p.xi.aj <- gdina_calc_individual_likelihood( IP=IP, L=L, pjM=pjM, item.patt.split=item.patt.split,
                        J=J, resp.ind.list=resp.ind.list, zeroprob.skillclasses=zeroprob.skillclasses )
    #--- compute attribute probabilities
    attr.prob <- IRT_se_gdina_calc_skill_distribution( beta=beta, Z.skillspace=Z.skillspace,
                        reduced.skillspace=reduced.skillspace, G=G, eps=1E-5 )
    eps <- 1E-30
    #--- calculate the updated likelihood
    p.xi.aj[ p.xi.aj > 1 ] <- 1 - eps
    p.xi.aj[ p.xi.aj < 0 ] <- eps

    if (G==1){
        l1 <- rowSums( p.xi.aj * cdm_matrix2( attr.prob , nrow=IP ) ) + eps
        l1[ l1 < 0 ] <- eps
        like_ind <- l1
        loglike <- sum( log(l1) * item.patt.freq )
    }
    if (G>1){
        like_ind <- array( 0 , dim=c(IP, L, G) )
        loglike <- 0
        for (gg in 1:G){
            l1 <- rowSums( p.xi.aj * cdm_matrix2( attr.prob[,gg] , nrow=IP ) ) + eps
            l1[ l1 < 0 ] <- eps
            like_ind[,,gg] <- l1
            loglike <- loglike + sum( log(l1) * item.patt.freq[,gg] )
        }
    }

    #--- update posterior if requested
    if (return_all){
        res <- gdina_calc_individual_posterior(G=G, IP=IP, attr.prob=attr.prob, p.xi.aj=p.xi.aj, L=L,
                    I=0, zeroprob.skillclasses=zeroprob.skillclasses,
                    reduced.skillspace=reduced.skillspace, item.patt.freq=item.patt.freq)
        p.aj.xi <- res$p.aj.xi
    }
    #--- output
    res <- loglike
    if (return_all){
        res <- list( loglike=loglike, p.xi.aj=p.xi.aj, p.aj.xi=p.aj.xi, like_ind = like_ind  )
    }

    return(res)
}
