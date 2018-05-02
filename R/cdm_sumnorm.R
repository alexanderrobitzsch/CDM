## File Name: cdm_sumnorm.R
## File Version: 0.02

cdm_sumnorm <- function(vec, norm=1)
{
    vec <- as.vector(vec)
    res <- vec / sum(vec) * norm
    return(res)
}
