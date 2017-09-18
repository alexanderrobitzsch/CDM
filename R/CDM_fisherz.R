## File Name: CDM_fisherz.R
## File Version: 0.01
## File Last Change: 2017-07-11 10:02:26


## copy from psych::fisherz
CDM_fisherz <- function(rho) 
{
    0.5 * log((1 + rho)/(1 - rho))
}
