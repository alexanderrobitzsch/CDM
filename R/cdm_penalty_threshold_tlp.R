## File Name: cdm_penalty_threshold_tlp.R
## File Version: 0.06

cdm_penalty_threshold_tlp <- function( beta, tau, lambda )
{
    lambda_j <- lambda / tau * ( abs(beta) < tau )
    res <- cdm_penalty_threshold_lasso( val=beta, eta=lambda_j)
    return(res)
}
