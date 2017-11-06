## File Name: cdm_penalty_tlp_approximation.R
## File Version: 0.07

cdm_penalty_tlp_approximation <- function(x, tau, lambda )
{
	y <- abs(x) / tau	
	J1 <- y
	# J2 <- y - ifelse(  y - 1 > 0, y - 1 , 0 )	
	J2 <- y - ifelse( y - 1 < 0 , y , 1 )
	z <- lambda*(J1 - J2)	
	return(z)
}
