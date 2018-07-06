## File Name: gdina_mstep_item_parameters_number_of_regularized_parameters.R
## File Version: 0.02


gdina_mstep_item_parameters_number_of_regularized_parameters <- function( regularization,
    delta, J)
{
    #--- regularized parameters
    numb_regular_pars <- NA
    if (regularization){
        numb_regular_pars <- 0
        eps <- 1E-4
        for (jj in 1:J){
            delta_jj <- delta[[jj]]
            numb_regular_pars <- numb_regular_pars + sum( abs( delta_jj ) < eps )
        }
    }
    return(numb_regular_pars)
}
