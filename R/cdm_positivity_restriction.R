## File Name: cdm_positivity_restriction.R
## File Version: 0.01

cdm_positivity_restriction <- function(x, positive)
{
	x <- ifelse( ( x < 0 ) & positive , 0 , x )
	return(x)
}
