## File Name: cdm_lasso_threshold.R
## File Version: 0.02

cdm_lasso_threshold <- function( val, eta )
{
    res <- val
    res <- ifelse( abs(val) < eta , 0 , res )
    res <- ifelse( val > eta , val - eta , res )
    res <- ifelse( val < - eta , val + eta , res )
    return(res)
}

cdm_soft_threshold <- cdm_lasso_threshold
