## File Name: cdm_pem_acceleration_assign_output_parameters.R
## File Version: 0.02

cdm_pem_acceleration_assign_output_parameters <- function(res_ll_fct, vars, envir, update)
{
    if (update){
        for (vv in vars){
            assign( vv , res_ll_fct[[ vv ]] , envir=envir )
        }
    }
}
