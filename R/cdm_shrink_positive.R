## File Name: cdm_shrink_positive.R
## File Version: 0.02

cdm_shrink_positive <- function(x, sum1=TRUE)
{
    y <- ifelse(x<0, 0, x)
    if (sum1){
        y <- y / sum(y)
    }
    return(y)
}
