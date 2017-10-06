## File Name: slca_set_seed.R
## File Version: 0.02
## File Last Change: 2017-10-05 18:53:05

slca_set_seed <- function(seed)
{
	seed.used <- NULL
	if ( ! is.null(seed) ){
		seed.used <- seed
		set.seed( seed=seed.used )
	}	
	#--- output
	res <- list( seed.used=seed.used )
	return(res)
}
