## File Name: chisq_compute.R
## File Version: 0.02

chisq_compute <- function(obs, exp)
{
    chisq <- sum( ( obs - exp)^2 / exp )
    return(chisq)
}
