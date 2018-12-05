## File Name: cdm_timecat.R
## File Version: 0.02

cdm_timecat <- function(z0, label, print=FALSE)
{
    if (print){
        cat(label) ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1
    }
    return(z0)
}
