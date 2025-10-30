## File Name: cdm_ginv.R
## File Version: 0.04

cdm_ginv <- function(x,...)
{
    CDM_require_namespace('MASS')
    y <- MASS::ginv(X=x)
    return(y)
}
