## File Name: cdm_rowSums.R
## File Version: 0.031

cdm_rowSums <- function(x)
{
    if (is.matrix(x)){
        res <- rowSums(x)
    } else {
        res <- x
    }
    return(res)
}
