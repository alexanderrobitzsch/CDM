## File Name: cdm_add_ridge_diagonal.R
## File Version: 0.02

cdm_add_ridge_diagonal <- function(x , eps=1E-10 )
{
    D <- ncol(x)
    rx <- x + diag( eps , D )
    return(rx)
}

