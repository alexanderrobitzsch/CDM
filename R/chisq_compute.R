## File Name: chisq_compute.R
## File Version: 0.01

chisq_compute <- function(obs, exp)
{ 
	chisq <- sum( ( obs - exp)^2 / exp )
	return(chisq)
}
