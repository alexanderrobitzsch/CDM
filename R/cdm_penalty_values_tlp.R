## File Name: cdm_penalty_values_tlp.R
## File Version: 0.04

cdm_penalty_values_tlp <- function(x, tau )
{
    y <- abs(x) / tau
    y <- ifelse( y > 1, 1, y )
    return(y)
}
