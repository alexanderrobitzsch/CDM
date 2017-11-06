## File Name: reglca_init_parameters_item_probs.R
## File Version: 0.01


reglca_init_parameters_item_probs <- function( qmeans, I, nclasses, sd_noise_init, parm_range=1 )
{
	I <- length(qmeans)
	item_probs <- matrix( NA, nrow=I, ncol=nclasses)
	for (ii in 1:I){
		item_probs[ii,] <- qmeans[ii] + parm_range*seq(-1,1, length=nclasses) + stats::rnorm( nclasses, mean=0, sd=sd_noise_init )
	}
	item_probs <- stats::pnorm( item_probs )
	return(item_probs)
}
	
