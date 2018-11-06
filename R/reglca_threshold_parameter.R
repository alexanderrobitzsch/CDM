## File Name: reglca_threshold_parameter.R
## File Version: 0.09

reglca_threshold_parameter <- function(x, regular_type, lambda, vt=1 )
{
    # multiply <- TRUE
    multiply <- ( vt < 1 )
    if (multiply){
        x <- x * vt
    }
    y <- cdm_parameter_regularization(x=x, regular_type=regular_type, regular_lam=lambda)
    if (multiply){
        y <- y / vt
    }
    return(y)
}
