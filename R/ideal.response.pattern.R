## File Name: ideal.response.pattern.R
## File Version: 0.07


#########################################################################
# computation of ideal response pattern
# main function
ideal.response.pattern <- function( q.matrix , skillspace=NULL )
{
	K <- ncol(q.matrix)
	if ( is.null(skillspace) ){
		skillspace <- data.frame( rbind( rep(0,K) , rep(1,K) ) )
		skillspace <- as.matrix( expand.grid( as.list( skillspace ) ) )
		if ( ! is.null( colnames(q.matrix) ) ){   
			colnames(skillspace) <- colnames(q.matrix) 
		}
	}
	# compute ideal response pattern
	skillspace <- as.matrix(skillspace)
	q.matrix <- as.matrix(q.matrix)
	idealresp <- cdm_rcpp_ideal_resp_pattern( qmatrix=q.matrix, skillspace=skillspace )
	res <- list( "idealresp"= idealresp , "skillspace" = skillspace )
	return(res)
}
#########################################################################		

