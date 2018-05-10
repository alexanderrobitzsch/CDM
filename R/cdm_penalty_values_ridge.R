## File Name: cdm_penalty_values_ridge.R
## File Version: 0.02

cdm_penalty_values_ridge <- function( x, lambda )
{
    y <- lambda * x^2
    return(y)
}
