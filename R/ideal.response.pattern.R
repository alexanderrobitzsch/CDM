

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
    idealresp <- ideal_resp_pattern__Cpp( q.matrix , skillspace )    
    res <- list( "idealresp"= idealresp , "skillspace" = skillspace )
    return(res)
}
#########################################################################		


###############################################################################
ideal_resp_pattern__Cpp <- function( q.matrix , skillspace )
{
    skillspace <- as.matrix(skillspace)
	q.matrix <- as.matrix(q.matrix)
	res <- ideal_resp_pattern__C( q.matrix , skillspace )
    return(res)			
}
###############################################################################
