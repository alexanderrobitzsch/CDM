## File Name: cdm_penalty_values_lasso.R
## File Version: 0.05

cdm_penalty_values_lasso <- function( x, lambda )
{
    y <- lambda * abs(x)
    return(y)
}
