## File Name: abs_approx_D1.R
## File Version: 0.01


abs_approx_D1 <- function( x , eps = 1E-5){
    res <-  x / sqrt( x^2 + eps )
    return(res)
}
