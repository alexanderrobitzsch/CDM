## File Name: din.validate.qmatrix.R
## File Version: 1.415


#---- Q-matrix validation based on the DINA model
din.validate.qmatrix <- function( object, IDI_diff = .05, print=TRUE )
{
	s1 <- Sys.time()
	#--- extract original Q-matrix
	q.matrix <- object$q.matrix
	rule <- object$rule
	# extract estimated parameters
	guess <- object$guess[,1]
	slip <- object$slip[,1]
	# calculate originally estimated IDI
	IDI <- 1 - slip - guess
	#****
	# define all possible Q-matrix vectors
	nodes <- c(0,1)
	K <- ncol(q.matrix)
	L <- 2^K
	q.matrix.poss <- as.matrix( expand.grid( as.data.frame( matrix( rep(nodes, K) , ncol = K ) ) ) )	
	colnames(q.matrix.poss) <- colnames(q.matrix)
	q.matrix.poss <- q.matrix.poss[ ! ( rowMeans( q.matrix.poss ) %in% c(0) ) , ]
	QQM <- nrow(q.matrix.poss) 	# number of possible Q-matrix vectors
	#****
	# extract data and attributes
	data <- object$data	
	I <- ncol(data)		# number of items
	I.lj <- object$I.lj
	R.lj <- object$R.lj
	attr.patt <- object$attribute.patt.splitted
	# calculate modification item parameters
	coef.modified <- matrix( 0 , nrow=QQM*I , ncol=4 )
	colnames(coef.modified) <- c("item" , "qmatrix.row" , "guess" , "slip" )
	coef.modified <- as.data.frame( coef.modified )
	coef.modified$item <- rep( 1:I , QQM )
	coef.modified$qmatrix.row <- rep( 1:QQM , each=I )	
	
	#-- Q-matrix validation core function
	res <- cdm_rcpp_din_validate_update_qmatrix( qmatrix_poss=q.matrix.poss, 
				attr_patt=attr.patt, Ilj=I.lj, Rlj=R.lj, I=I, L=L, K=K ) 				
	coef.modified$guess <- res$guess_M
	coef.modified$slip <- res$slip_M

	coef.modified <- coef.modified[ order( coef.modified$item ) , ]	
	coef.modified$IDI <- 1 - coef.modified$slip - coef.modified$guess

	# look for original rows
	coef.modified$qmatrix.orig <- 1 * ( rowMeans( q.matrix.poss[ coef.modified$qmatrix.row , ] 
				==	q.matrix[ coef.modified$item , ] ) == 1 )

	coef.modified$IDI.orig <- IDI[ coef.modified$item ]
	coef.modified$delta.IDI <- coef.modified$IDI - coef.modified$IDI.orig
	
	# restructure matrix coef.modified
	coef.modified <- data.frame( item = colnames(data)[ coef.modified$item ] , 
				itemindex = coef.modified$item ,
				q.matrix.poss[ coef.modified$qmatrix.row , ] ,
				coef.modified[ , - c(1:2) ] )	
	
	#-- calculate maximum delta index per item	
	# a1 <- stats::aggregate( coef.modified$IDI , list( coef.modified$itemindex ) , max )
	a1 <- cdm_rcpp_din_validate_aggregate_max( IDI=coef.modified$IDI,
					itemindex=coef.modified$itemindex, I=I)				

	coef.modified$max.IDI <- a1[ coef.modified$itemindex , 2]
	coef.modified <- coef.modified[ order( coef.modified$itemindex - coef.modified$IDI ) , ]	
	
	# print output
	coef.modified2 <- coef.modified
	improve <- coef.modified2$IDI - coef.modified$IDI.orig > IDI_diff
	coef.modified2 <- coef.modified2[ improve, ]
	nochange <- nrow(coef.modified2) == 0
	# calculate proposed Q-matrix
	q.matrix.prop <- q.matrix
	if ( ! nochange ){ 
		items <- unique( coef.modified2$itemindex )
		for (ii in items ){
			c2 <- ( coef.modified2[ coef.modified2$itemindex == ii , ] )[1,]
			q.matrix.prop[ ii , ] <- as.vector( t(c2[ 1, seq(3 , 3+K -1) ]) )
		}
	}	
		
	if ( print ){
		if ( ! nochange ){ 		
			print( coef.modified2 ) 
			cat("\nProposed Q-matrix:\n\n")
			print(q.matrix.prop)
		}
		if ( nochange ){ cat("No Q-matrix entries should be changed.\n") }
	}	
	
	s2 <- Sys.time()
	res <- list( coef.modified = coef.modified ,
				coef.modified.short = coef.modified2 ,
				q.matrix.prop = q.matrix.prop, time_diff = s2 - s1 )
	class(res) <- "din.validate.qmatrix"
	return(res)
}
######################################################

# cat("* loop") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1	
