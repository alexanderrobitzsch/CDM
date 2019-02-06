## File Name: cdm_create_vector.R
## File Version: 0.01

cdm_create_vector <- function(names, val)
{
    N <- length(names)
    y <- rep(val, N)
    names(y) <- names
    return(y)
}
