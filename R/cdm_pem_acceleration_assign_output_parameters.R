## File Name: cdm_pem_acceleration_assign_output_parameters.R
## File Version: 0.01
## File Last Change: 2017-10-05 17:26:09

cdm_pem_acceleration_assign_output_parameters <- function(res_ll_fct, vars, envir)
{
	for (vv in vars){
		assign( vv , res_ll_fct[[ vv ]] , envir=envir )
	}
}
