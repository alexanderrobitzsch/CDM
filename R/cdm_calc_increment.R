## File Name: cdm_calc_increment.R
## File Version: 0.08
## File Last Change: 2017-10-08 15:16:43

cdm_calc_increment <- function( d1, d2, max.increment , eps = 1E-10, adj_fac = .98, type=1 )
{
	increment <- d1 / ( abs( d2 + eps ) )
	increment[ is.na(increment) ] <- 0	
	#--- vector increment
	if (is.vector(increment) ){	
		increment <- cdm_trim_increment( increment=increment, max.increment=max.increment, type=type ) 
	}	
	#--- matrix increment
	if (is.matrix(increment) ){	
		K <- ncol(increment)
		for (kk in 1:K){
			increment[,kk] <- cdm_trim_increment( increment=increment[,kk], max.increment=max.increment , type=type) 
		}
	}	
    #--- adjust maximum increment		
	max.increment <- max(abs(increment)) / adj_fac
	#--- output
	res <- list(increment=increment, max.increment=max.increment)	
	return(res)
}
