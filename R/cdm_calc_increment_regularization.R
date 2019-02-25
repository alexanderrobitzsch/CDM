## File Name: cdm_calc_increment_regularization.R
## File Version: 0.363

cdm_calc_increment_regularization <- function( d1, d2, x0, regular_lam_used,
        max.increment, regular_type, regular_n=1, eps=1E-10, adj_fac=.98)
{
    d2a <- ( abs(d2) + eps )
    val <- x0 + d1 / d2a
    vt <- d2a
    val1 <- vt*val
    regular_lam_used <- regular_n * regular_lam_used
    updated <- cdm_parameter_regularization(x=val1, regular_type=regular_type,
                        regular_lam=regular_lam_used)
    updated <- updated / vt
    increment <- updated - x0
    max.increment <- 3
    increment <- cdm_trim_increment( increment=increment, max.increment=max.increment )
    updated <- x0 + increment
    #- compute indicator for regularized estimate
    parm_regularized <- ( regular_lam_used > 0 ) & ( abs(updated) < eps )
    numb_regularized <- sum(parm_regularized)
    max.increment <- max(abs(increment)) / adj_fac
    #--- output
    res <- list( increment=increment, updated=updated, max.increment=max.increment,
                    parm_regularized=parm_regularized, numb_regularized=numb_regularized)
    return(res)
}

