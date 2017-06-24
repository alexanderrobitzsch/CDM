
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