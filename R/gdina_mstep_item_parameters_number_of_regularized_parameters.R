## File Name: gdina_mstep_item_parameters_number_of_regularized_parameters.R
## File Version: 0.06


gdina_mstep_item_parameters_number_of_regularized_parameters <- function( regularization,
    delta, J, eps=1e-6)
{
    #--- regularized parameters
    numb_regular_pars <- NA
    delta_regularized <- list()
    if (regularization){
        numb_regular_pars <- 0
        for (jj in 1:J){
            delta_jj <- delta[[jj]]
            delta_reg <- abs( delta_jj ) < eps
            delta_reg[1] <- FALSE
            delta_regularized[[jj]] <- delta_reg
            numb_regular_pars <- numb_regular_pars + sum(delta_regularized[[jj]])
        }
    }
    #--- output
    res <- list(numb_regular_pars=numb_regular_pars, delta_regularized=delta_regularized)
    return(res)
}
