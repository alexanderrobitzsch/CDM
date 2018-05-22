## File Name: cdm_penalty_threshold_mcp.R
## File Version: 0.04

cdm_penalty_threshold_mcp <- function(beta, lambda, a=3.7)
{
    y <- cdm_penalty_threshold_lasso(val=beta, eta=lambda ) / ( 1 -  1/ a )
    y <- ifelse( abs(beta) > a*lambda, beta, y )
    return(y)
}
