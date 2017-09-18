## File Name: gdina_mstep_item_parameters_designmatrix.R
## File Version: 0.02
## File Last Change: 2017-06-17 14:07:19

gdina_mstep_item_parameters_designmatrix <- function( delta.new , delta.designmatrix ,
		delta.basispar.lower, delta.basispar.upper, Mj.index, J )
{	
	u.delta.new <- unlist( delta.new )
	# calculate basis parameter of delta
	delta.basispar <- solve( t( delta.designmatrix) %*% delta.designmatrix ) %*% 
								t(delta.designmatrix) %*% u.delta.new
	if ( ! is.null( delta.basispar.lower )){						
		delta.basispar <- ifelse( delta.basispar < delta.basispar.lower , 
									delta.basispar.lower , delta.basispar )
	}
	if ( ! is.null( delta.basispar.upper )){						
		delta.basispar <- ifelse( delta.basispar > delta.basispar.upper , 
									delta.basispar.upper , delta.basispar )
	}
	delta.new1 <- ( delta.designmatrix %*% delta.basispar )[,1]
	for (jj in 1:J){
		delta.new[[jj]] <- delta.new1[ seq( Mj.index[jj,2] , Mj.index[jj,3] ) ]
	}
	return(delta.new)
}
