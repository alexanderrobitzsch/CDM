## File Name: cdm_sumnorm_squeeze.R
## File Version: 0.01

cdm_sumnorm_squeeze <- function(vec, bounds)
{
    y <- cdm_sumnorm( vec=cdm_squeeze(x=vec, bounds=bounds) )
    return(y)
}
