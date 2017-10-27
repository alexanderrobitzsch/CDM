## File Name: cdm_include_fixed_parameters.R
## File Version: 0.03

cdm_include_fixed_parameters <- function( parm, se_parm, parm_fixed )
{
	#--- vector of parameters
	if ( is.vector(parm) ){
		if ( ! is.null( parm_fixed ) ){
			parm[ parm_fixed[,1] ] <- parm_fixed[,2]
			se_parm[ parm_fixed[,1] ] <- 0		
		}	
	}
	#--- matrix of parameters
	if ( is.matrix(parm) ){
		if ( ! is.null( parm_fixed ) ){
			parm[ parm_fixed[,1:2,drop=FALSE] ] <- parm_fixed[,3,drop=FALSE]
			se_parm[ parm_fixed[,1:2,drop=FALSE] ] <- 0		
		}
	}	
	#-------------------------------------
	#--- output
	res <- list( parm=parm, se_parm=se_parm)
	return(res)
}
