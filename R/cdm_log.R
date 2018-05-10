## File Name: cdm_log.R
## File Version: 0.04

cdm_log <- function(x, eps)
{
    x <- ifelse(x < eps, eps, x)
    y <- log(x)
    return(y)
}
