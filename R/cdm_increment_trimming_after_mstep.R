## File Name: cdm_increment_trimming_after_mstep.R
## File Version: 0.02


cdm_increment_trimming_after_mstep <- function( parm, parm0, max.increment0, type )
{
    parm <- parm0 + cdm_trim_increment( increment = parm - parm0 , max.increment=max.increment0, type=2 )
    max.increment0 <- min( max.increment0 , max(abs(parm - parm0)) )
    #--- output
    res <- list( parm=parm, max.increment0=max.increment0)
    return(res)
}
