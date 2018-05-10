## File Name: cdm_penalty_threshold_ridge.R
## File Version: 0.03

cdm_penalty_threshold_ridge <- function(beta, lambda)
{
    y <- beta / ( 1 + 2*lambda )
    return(y)
}
