## File Name: cdm_mcp_threshold.R
## File Version: 0.01

cdm_mcp_threshold <- function(beta, lambda, a=3.7)
{
	y <- cdm_lasso_threshold(val=beta, eta=lambda ) / ( 1 -  1/ a )
	y <- ifelse( abs(beta) > a*lambda , beta , y )
	return(y)
}
