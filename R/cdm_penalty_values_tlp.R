## File Name: cdm_penalty_values_tlp.R
## File Version: 0.03

cdm_penalty_values_tlp <- function(x, tau )
{
    y <- abs(x) / tau
    y <- ifelse( y > 1 , 1 , y )
    return(y)
}
