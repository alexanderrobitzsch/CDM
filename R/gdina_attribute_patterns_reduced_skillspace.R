## File Name: gdina_attribute_patterns_reduced_skillspace.R
## File Version: 0.02

gdina_attribute_patterns_reduced_skillspace <- function( attr.patt, K, maxAttr, q.matrix,
		Z.skillspace )
{
	A <- attr.patt
	# combinations
	kombis <- utils::combn( K , 2 )	
	KK <- ncol(kombis)
	B <- NULL
	for (kk in 1:KK){
		B <- cbind( B , attr.patt[ , kombis[1,kk] ] * attr.patt[ , kombis[2,kk] ] )
	}
    Z <- cbind( 1 , A , B )
	ncolZ <- ncol(Z)
    v1 <- c("Int" ,  paste("A",1:K , sep="") ) 		 
	v1 <- c(v1,apply( kombis , 2 , FUN = function(ll){ 
			paste( paste( "A" , ll , sep="") , collapse="_" ) } ))
	colnames(Z) <- v1	
	
	m1 <- which( maxAttr > 1 )
	if ( max(maxAttr) > 1 ){
	   Z1 <- Z[ , m1 , drop=FALSE ]^2
	   colnames(Z1) <- paste0( colnames(q.matrix)[m1] , "*2")
	   Z <- cbind( Z , Z1 )
	}
	if ( ! is.null(Z.skillspace) ){ 
		Z <- Z.skillspace
	}
	ncolZ <- ncol(Z)
	#----- OUTPUT
	res <- list(Z=Z, ncolZ=ncolZ )
	return(res)
}
