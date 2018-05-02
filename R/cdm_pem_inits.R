## File Name: cdm_pem_inits.R
## File Version: 0.05

cdm_pem_inits <- function( parmlist )
{
    pem_parameter_index <- cdm_pem_create_parameter_index( parmlist=parmlist )
    pem_parameter_sequence <- list()
    #--- output
    res <- list(pem_parameter_index=pem_parameter_index,
                pem_parameter_sequence=pem_parameter_sequence )
    return(res)
}
