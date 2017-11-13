## File Name: reglca_init_parameters.R
## File Version: 0.13

reglca_init_parameters <- function( nclasses, dat0, sd_noise_init, item_probs_init, class_probs_init,
		random_starts, G)
{
	use_random_starts <- FALSE
	means <- colMeans(dat0 , na.rm=TRUE )
	I <- ncol(dat0)
	#--- initial class probabilities
	if ( is.null(class_probs_init) ){
		class_probs <- reglca_init_parameters_class_probs( nclasses=nclasses, 
							sd_noise_init=sd_noise_init, G=G)
	} else {
		class_probs <- class_probs_init
		random_starts <- 1
	}
	
	#--- item probabilities
	qmeans <- stats::qnorm(means)
	if ( is.null(item_probs_init) ){	
		item_probs <- reglca_init_parameters_item_probs( qmeans=qmeans, I=I, nclasses=nclasses, 
								sd_noise_init=sd_noise_init, parm_range=1 ) 		
	} else {
		item_probs <- item_probs_init
		random_starts <- 1
	}
	
	#--- item probabilities in case of random starts
	if (random_starts > 1){
		item_probs <- list()
		sd_noise_init <- max( sd_noise_init , .01 )
		for (rr in 1:random_starts){
			item_probs[[rr]] <- reglca_init_parameters_item_probs( qmeans=qmeans, I=I, nclasses=nclasses, 
										sd_noise_init=sd_noise_init, parm_range=1 ) 	
		}
	}

	#--- class probabilities in case of random starts
	if (random_starts > 1){
		class_probs <- list()
		for (rr in 1:random_starts){
			class_probs[[rr]] <- reglca_init_parameters_class_probs( nclasses=nclasses, 
										sd_noise_init=sd_noise_init, G=G)
		}
		use_random_starts <- TRUE
	}	
	
	#--- output
	res <- list( class_probs=class_probs, item_probs=item_probs, random_starts=random_starts,
						use_random_starts=use_random_starts )													
	return(res)
}
