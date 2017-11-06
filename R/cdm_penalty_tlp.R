## File Name: cdm_penalty_tlp.R
## File Version: 0.02

cdm_penalty_tlp <- function(x, tau )
{
	y <- abs(x) / tau
	y <- ifelse( y > 1 , 1 , y )
	return(y)
}
