## File Name: CDM_fisherz.R
## File Version: 0.01


## copy from psych::fisherz
CDM_fisherz <- function(rho) 
{
    0.5 * log((1 + rho)/(1 - rho))
}
