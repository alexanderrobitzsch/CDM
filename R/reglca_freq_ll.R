## File Name: reglca_freq_ll.R
## File Version: 0.06

reglca_freq_ll <- function(x,C,W)
{
    eps <- 1E-20
    sum( C * log(x+eps) + W * log( 1 - x + eps) )
}
