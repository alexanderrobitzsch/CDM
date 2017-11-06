## File Name: reglca_init_parameters_class_probs.R
## File Version: 0.01



reglca_init_parameters_class_probs <- function( nclasses, sd_noise_init)
{
	parm <- stats::qnorm( rep( 1/nclasses, nclasses ) )
	parm <- parm + stats::rnorm( nclasses , sd = sd_noise_init )
	class_probs <- cdm_sumnorm( stats::pnorm( parm ) )
	return(class_probs)
}
	
