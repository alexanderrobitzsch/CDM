## File Name: cdm_matrix1.R
## File Version: 0.02

cdm_matrix1 <- function( x, ncol )
{
    x <- as.vector(x)
    y <- matrix( x, nrow=length(x), ncol=ncol, byrow=FALSE )
    return(y)
}
