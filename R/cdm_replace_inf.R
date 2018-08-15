## File Name: cdm_replace_inf.R
## File Version: 0.02

cdm_replace_inf <- function(x)
{
    x[ x == Inf ] <- NA
    return(x)
}
