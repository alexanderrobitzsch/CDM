
			
#################################################
# define Q matrix
gdm_Qmatrix <- function(Qmatrix,irtmodel,I,TD,K,a)
{
	# Q matrix [1:I , 1:TD , 1:K]
	if ( is.null(Qmatrix) ){
		Qmatrix <- array( 1 , dim=c(I,TD,K) )	
			# modify it possibly
		if (K>1 & ( irtmodel != "2PLcat" ) ){
			for (kk in 2:K){Qmatrix[,,kk] <- kk*Qmatrix[,,1] }
		}				
		if ( irtmodel=="2PLcat"){
			for (kk in 2:K ){
				a[,,kk] <- kk * a[,,kk]
				}
			}
		}
	res <- list(Qmatrix = Qmatrix , a = a)
	return(res)
}			
			