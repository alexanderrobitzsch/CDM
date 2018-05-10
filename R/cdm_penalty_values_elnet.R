## File Name: cdm_penalty_values_elnet.R
## File Version: 0.02

cdm_penalty_values_elnet <- function( x, lambda, alpha )
{
    lam1 <- lambda * alpha
    lam2 <- lambda * ( 1 - alpha )
    y <- lam1 * abs(x) + lam2 * x^2
    return(y)
}
