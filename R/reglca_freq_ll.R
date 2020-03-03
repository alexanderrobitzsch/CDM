## File Name: reglca_freq_ll.R
## File Version: 0.161

reglca_freq_ll <- function(x, C, W, eps=1e-20)
{
    res <- sum( C * cdm_log(x, eps) + W * cdm_log( 1-x, eps) )
    return(res)
}
