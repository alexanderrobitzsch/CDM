## File Name: reglca_threshold_parameter.R
## File Version: 0.05

reglca_threshold_parameter <- function(x, regular_type, lambda, vt=1 )
{
    if (vt < 1){
        x <- x * vt
    }
    y <- cdm_parameter_regularization(x=x, regular_type=regular_type, regular_lam=lambda)
    if (vt < 1){
        y <- y / vt
    }
    return(y)
}
