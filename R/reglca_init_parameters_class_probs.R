## File Name: reglca_init_parameters_class_probs.R
## File Version: 0.07

reglca_init_parameters_class_probs <- function( nclasses, sd_noise_init, G)
{
    parm <- stats::qnorm( rep( 1/nclasses, nclasses ) )
    parm <- parm + stats::rnorm( nclasses, sd=sd_noise_init )
    class_probs <- cdm_sumnorm( stats::pnorm( parm ) )
    if (G > 1){
        class_probs <- matrix( class_probs, nrow=nclasses, ncol=G, byrow=FALSE)
    }
    return(class_probs)
}

