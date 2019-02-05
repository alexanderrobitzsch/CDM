## File Name: cdm_shrink_positive.R
## File Version: 0.01

cdm_shrink_positive <- function(x)
{
    y <- ifelse(x<0, 0, x)
    y <- y / sum(y)
    return(y)
}
