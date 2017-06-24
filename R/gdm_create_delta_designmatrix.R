


########################################################
# create delta design matrix
gdm_create_delta_designmatrix <- function( delta.designmatrix , 
		TP , D , theta.k , skill.levels,G)
{
	delta.designmatrix <- rep(1,TP)
	for (dd in 1:D){
		for ( pp in 1:(min( skill.levels[dd]-1 ,3) ) ){
			delta.designmatrix <- cbind( delta.designmatrix , theta.k[,dd]^pp )
		}
	}
	if (D>1){
		for (dd1 in 1:(D-1) ){				
			for (dd2 in (dd1+1):D) {					
				delta.designmatrix <- cbind( delta.designmatrix , theta.k[,dd1]*theta.k[,dd2] )		
			}
		}		
	}
	delta <- matrix(0,ncol(delta.designmatrix),G)
	covdelta <- NULL			
	res <- list( delta = delta , covdelta = covdelta , delta.designmatrix = delta.designmatrix )
	return(res)
}
		