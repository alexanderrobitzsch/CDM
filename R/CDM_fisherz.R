## File Name: cdm_fisherz.R
## File Version: 0.03


## copy from psych::fisherz
cdm_fisherz <- function(rho) 
{
	0.5 * log((1 + rho)/(1 - rho))
}
