## File Name: cdm_penalty_threshold_lasso.R
## File Version: 0.04

cdm_penalty_threshold_lasso <- function( val, eta )
{
    res <- val
    res <- ifelse( abs(val) < eta , 0 , res )
    res <- ifelse( val > eta , val - eta , res )
    res <- ifelse( val < - eta , val + eta , res )
    return(res)
}

cdm_soft_threshold <- cdm_penalty_threshold_lasso
