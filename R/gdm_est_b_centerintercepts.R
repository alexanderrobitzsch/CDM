## File Name: gdm_est_b_centerintercepts.R
## File Version: 0.01
## File Last Change: 2017-10-08 13:51:41

gdm_est_b_centerintercepts <- function(b, centerintercepts, TD, Qmatrix )
{
	if ( centerintercepts ){
		if (TD==1){
			b <- b - mean(b)		
		}
		if (TD > 1){		
			for (dd in 1:TD){
				ind.dd <- which( Qmatrix[,dd,1] > 0 )
				m1 <- sum( b[ind.dd,] ) / ( ncol(b) * length(ind.dd) )	
				b[ind.dd,] <- b[ind.dd,] - 	m1
			}
		}
	}
	return(b)
}
