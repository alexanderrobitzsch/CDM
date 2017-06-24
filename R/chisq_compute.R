
chisq_compute <- function(obs, exp)
{ 
	chisq <- sum( ( obs - exp)^2 / exp )
	return(chisq)
}