## File Name: cdm_calc_increment_regularization.R
## File Version: 0.12

cdm_calc_increment_regularization <- function( d1, d2, x0, regular_lam_used, max.increment, eps=1E-10, adj_fac=.98,
            regular_type)
{
    val <- x0 + d1 / abs( d2 + eps )
    updated <- cdm_parameter_regularization(x=val, regular_type=regular_type, regular_lam=regular_lam_used)
    increment <- updated - x0
    max.increment <- 3
    increment <- cdm_trim_increment( increment=increment, max.increment=max.increment )
    updated <- x0 + increment
    max.increment <- max(abs(increment)) / adj_fac
    #--- output
    res <- list( increment=increment, updated=updated, max.increment=max.increment )
    return(res)
}

