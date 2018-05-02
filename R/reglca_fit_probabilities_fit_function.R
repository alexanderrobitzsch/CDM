## File Name: reglca_fit_probabilities_fit_function.R
## File Version: 0.06


reglca_fit_probabilities_fit_function <- function( parm, lambda , C, W, regular_type="scad")
{
    probs0 <- reglca_calc_probs(parm=parm)
    ll0 <- reglca_freq_ll( x=probs0, C=C, W=W )
    pen0 <- - sum( cdm_penalty_values(x=parm[-1], regular_type=regular_type, regular_lam=lambda) )
    opt0 <- ll0 + pen0
    #--- output
    res <- list( ll=ll0, pen=pen0, fit_fct=opt0)
    return(res)
}
