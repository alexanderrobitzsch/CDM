## File Name: gdina_create_designmatrices_monotonicity_constraints.R
## File Version: 0.01

gdina_create_designmatrices_monotonicity_constraints <- function(Ajjj)
{
		
	N_Ajjj <- nrow(Ajjj)
	Ajjj_sum <- rowSums(Ajjj)
		
	# compute distance matrix
	dist_Ajjj <- matrix( -9, nrow=N_Ajjj, ncol=N_Ajjj )
	for (ii in 1:(N_Ajjj-1) ){
		for (hh in (ii+1):N_Ajjj){
			dist_Ajjj[hh,ii] <- sum( abs( Ajjj[hh,] - Ajjj[ii,] ) )
		}
	}
	# define matrix for testing monotonicity constraints
	NC <- sum( dist_Ajjj == 1 )
	Ajjj_mono <- matrix( 0 , nrow=NC, ncol=N_Ajjj)
	uu <- 1
	for (ii in 1:N_Ajjj ){
		for (hh in 1:N_Ajjj){
			if ( dist_Ajjj[hh,ii] == 1 ){
				Ajjj_mono[uu,c(hh,ii) ] <- c(1,-1)
				uu <- uu + 1
			}
		}
	}		
	return(Ajjj_mono)
}
