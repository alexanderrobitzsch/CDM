## File Name: gdina_mstep_item_ml_update_parameter_regularization.R
## File Version: 0.23


gdina_mstep_item_ml_update_parameter_regularization <- function(x, regular_type, regular_lam,
        regular_alpha, regular_tau, vt=1)
{
    x <- x*vt
    y <- cdm_parameter_regularization(x=x, regular_type=regular_type, regular_lam=regular_lam,
                regular_alpha=regular_alpha, regular_tau=regular_tau )
    y <- y / vt
    return(y)
}
