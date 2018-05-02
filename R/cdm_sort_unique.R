## File Name: cdm_sort_unique.R
## File Version: 0.01

cdm_sort_unique <- function(x)
{
    if( ! ( is.numeric(x) ) ){
        gr2 <- unique( sort(paste(x) ))
    } else {
        gr2 <- unique( sort(x) )
    }
    return(gr2)
}
