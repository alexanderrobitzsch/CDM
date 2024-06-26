## File Name: gdina_calc_individual_likelihood.R
## File Version: 0.072

gdina_calc_individual_likelihood <- function(IP, L, pjM, item.patt.split, J,
        resp.ind.list, zeroprob.skillclasses, ones_matrix=NULL)
{
    if ( is.null(ones_matrix) ){
        ones_matrix <- matrix( 1, nrow=IP, ncol=L )
    }
    p.xi.aj <- cdm_calc_posterior( rprobs=pjM, gwt=ones_matrix, resp=item.patt.split,
                    nitems=J, resp.ind.list=resp.ind.list, normalization=FALSE,
                    thetasamp.density=NULL, snodes=0 )$hwt
    if ( ! is.null(zeroprob.skillclasses) ){
        p.xi.aj[, zeroprob.skillclasses ] <- 0
    }
    #--- OUTPUT
    return(p.xi.aj)
}
