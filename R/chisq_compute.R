## File Name: chisq_compute.R
## File Version: 0.01
## File Last Change: 2017-06-20 16:05:25

chisq_compute <- function(obs, exp)
{ 
	chisq <- sum( ( obs - exp)^2 / exp )
	return(chisq)
}
