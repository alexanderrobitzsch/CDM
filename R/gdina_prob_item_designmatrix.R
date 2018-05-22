## File Name: gdina_prob_item_designmatrix.R
## File Version: 0.06

gdina_prob_item_designmatrix <- function( delta_jj, Mjjj, linkfct, eps_squeeze )
{
    irf1 <- ( Mjjj %*% delta_jj )[,1]
    if ( linkfct=="log"){
        irf1 <- exp(irf1)
    }
    if ( linkfct=="logit"){
        irf1 <- stats::plogis(irf1)
    }
    irf1 <- cdm_squeeze( irf1, c(eps_squeeze, 1-eps_squeeze) )
    return(irf1)
}
