## File Name: cdm_positivity_restriction.R
## File Version: 0.01
## File Last Change: 2017-10-08 12:38:24

cdm_positivity_restriction <- function(x, positive)
{
	x <- ifelse( ( x < 0 ) & positive , 0 , x )
	return(x)
}
