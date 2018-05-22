## File Name: cdm_penalty_values_tlp_approximation.R
## File Version: 0.10

cdm_penalty_values_tlp_approximation <- function(x, tau, lambda )
{
    y <- abs(x) / tau
    J1 <- y
    J2 <- y - ifelse( y - 1 < 0, y, 1 )
    z <- lambda*(J1 - J2)
    return(z)
}
