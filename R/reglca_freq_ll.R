## File Name: reglca_freq_ll.R
## File Version: 0.07

reglca_freq_ll <- function(x,C,W)
{
    eps <- 1E-20
    sum( C * cdm_log(x, eps) + W * cdm_log( 1 - x , eps) )
}
