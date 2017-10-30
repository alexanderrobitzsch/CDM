## File Name: gdina_create_designmatrices.R
## File Version: 0.04

gdina_create_designmatrices <- function( J, Mj, Aj, q.matrix, rule, L, attr.patt, mono.constr )
{
	Mj.userdefined <- TRUE
	if ( is.null(Mj) ){ 
		Mj.userdefined <- FALSE
		Mj <- as.list( rep("noe" , J ) ) 
	}
	# Notation for Mj and Aj follows De La Torre (2011)
	Aj <- NULL
	Aj_mono_constraints <- NULL
	Nattr.items <- rowSums(q.matrix >= 1)
	# list of necessary attributes per item
	necc.attr <- as.list( rep(NA,J) )
	# list of rows in attr.patt which correspond to attribute classes for one item
	attr.items <- NULL
	# list of indices of attribute patterns which should be aggregated for each item
	aggr.attr.patt <- NULL
	for (jj in 1:J){ 	# loop over items jj
		nj1 <- necc.attr[[jj]] <- which( q.matrix[jj,] > 0 )
		if ( length(nj1)==0 ){ 
			stop( paste("Q matrix row " , jj , " has only zero entries\n" , sep="") ) 
		}	
		Aj1 <- Aj[[jj]] <- .create.Aj( Nattr.items[jj] )
		#--- define monotonicity constraints
		if (mono.constr){
			Aj_mono_constraints[[jj]] <- gdina_create_designmatrices_monotonicity_constraints(Ajjj=Aj[[jj]] )
		}
		if ( ! Mj.userdefined ){ 
			Mj[[jj]] <- .create.Mj( Aj[[jj]] , rule = rule[jj] )	
		}
 		l1 <- as.list( 1 )
		l2 <- rep(0,L)	
		for (zz in seq(1,nrow(Aj1)) ){  # begin row zz
 			Aj1zz <- outer( rep(1,nrow(attr.patt)) , Aj1[zz,] )
			apzz <- attr.patt[ , nj1 ]
			apzz <- 1 * ( apzz >= q.matrix[ rep(jj,L) ,nj1] )
			l1[[zz]] <- which( rowMeans( apzz == Aj1zz  ) == 1)
			l2[ l1[[zz]] ] <- zz
		}   # end row zz
		attr.items[[jj]] <- l1
		aggr.attr.patt[[jj]] <- l2		
	}	# end item jj

    #******						
	# indices for Mj
	Mj.index <- matrix( 0 , J , 6 )
	for (jj in 1:J){
		Mj.index[jj,1] <- ncol( Mj[[jj]][[1]] )	
		Mj.index[jj,4] <- nrow( Aj[[jj]])
	}
	Mj.index[,3] <- cumsum( Mj.index[,1] )
	Mj.index[,2] <- c(1,Mj.index[-J,3] + 1 )
	Mj.index[,6] <- cumsum( Mj.index[,4] )	
	Mj.index[,5] <- c(1,Mj.index[-J,6] + 1 )	
	# compute designmatrix of aggregation of pattern
	aggr.patt.designmatrix <- matrix( 0 , L , max(Mj.index[,6]) )
	for (jj in 1:J){
		Mj.index.jj <- Mj.index[jj,]
		for (vv in seq(1,Mj.index.jj[4]) ){
			aggr.patt.designmatrix[ , Mj.index.jj[5] - 1 + vv ] <- 1  * ( aggr.attr.patt[[jj]] == vv )
		}				
	}
	
	#--- OUTPUT
	res <- list(Mj=Mj, Mj.userdefined=Mj.userdefined, Aj=Aj, Nattr.items=Nattr.items, necc.attr=necc.attr,
				aggr.attr.patt=aggr.attr.patt, attr.items=attr.items, aggr.patt.designmatrix=aggr.patt.designmatrix,
				Mj.index=Mj.index, Aj_mono_constraints=Aj_mono_constraints )
	return(res)
}
