## File Name: expand_matrix.R
## File Version: 0.01
## File Last Change: 2017-06-20 15:57:03

expand_matrix <- function(x)
{
	NR <- nrow(x)
	NC <- ncol(x)
	NY <- max(NR,NC)
	y <- x
	if (NR!=NC){
		y <- matrix(0,nrow=NY,ncol=NY)
		if (NR < NC){
			y[ 1:NR, ] <- x
		}
		if (NR > NC){
			y[ , 1:NC ] <- x
		}		
	}
	return(y)
}
