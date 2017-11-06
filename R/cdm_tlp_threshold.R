## File Name: cdm_tlp_threshold.R
## File Version: 0.04

cdm_tlp_threshold <- function( beta, tau, lambda )
{
	lambda_j <- lambda / tau * ( abs(beta) < tau )
	res <- cdm_lasso_threshold( val=beta, eta=lambda_j)
	return(res)
}
