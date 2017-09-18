## File Name: gdina_proc_uls_inverse_matrices.R
## File Version: 0.01
## File Last Change: 2017-06-05 12:40:58

gdina_proc_uls_inverse_matrices <- function(Mj, J)
{
	invM.list <- list( 1:J )
	for (jj in 1:J){
		Mjjj <- Mj[[jj]][[1]]
		invM.list[[jj]] <- solve( crossprod(Mjjj) )
	}
	return(invM.list)
}
