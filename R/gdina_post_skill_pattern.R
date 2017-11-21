## File Name: gdina_post_skill_pattern.R
## File Version: 0.02

gdina_post_skill_pattern <- function( attr.prob, G, attr.patt.c, K, maxAttr, q.matrix,
		q.entries, attr.patt )
{
	# attribute pattern
	if (G==1){ 
		attr.prob <- matrix( attr.prob, ncol=1)
		colnames( attr.prob ) <- "class.prob"		
	}
	if (G>1){
		colnames( attr.prob ) <- paste( "class.prob.group" , 1:G , sep="")
	}
	rownames( attr.prob ) <- attr.patt.c	
	mA <- max(maxAttr)	
	if (G==1){   
		sp <- NULL 
		for (kk in 0:mA ){
			skill.patt <- matrix(apply( matrix( rep( attr.prob, K ), ncol=K) * 
					(attr.patt==kk), 2, sum ),ncol=1)
			rownames(skill.patt) <- colnames(q.matrix)
			colnames(skill.patt) <- paste0("skill.prob" ,kk )
			sp <- cbind( sp , skill.patt )
		}
		skill.patt <- sp
		for (kk in 1:K){ 
			ind.kk <- setdiff( 1:mA , 1 + q.entries[[kk]] )
			if ( length(ind.kk) > 0 ){ 
				skill.patt[ kk ,ind.kk ] <- NA 	
			}	
		}
	}
	if (G>1){   
		sp <- NULL
		for (kk in 0:( mA ) ){	
			skill.patt <- matrix( 0 , K , G )
			for (gg in 1:G){
			skill.patt[,gg] <- matrix(apply( matrix( rep(  attr.prob[,gg], K ), ncol=K) * 
										( attr.patt == kk ), 2, sum ),ncol=1)
			}
			rownames(skill.patt) <- colnames(q.matrix)
			colnames(skill.patt) <- paste0( "skill.prob" , kk , ".group"  , 1:G )				
			sp <- cbind( sp , skill.patt )
		}
		skill.patt <- sp
		for (kk in 1:K){ 
			v1 <- rep(1:mA,each=G)
			ind.kk <- setdiff( v1 , rep(1 + q.entries[[kk]],each=G) )
			ind.kk <- which( v1 %in% ind.kk )
			if ( length(ind.kk) > 0 ){ 
				skill.patt[ kk ,ind.kk ] <- NA 	
			}	
		}		
	}
	#---- OUTPUT
	res <- list( attr.prob=attr.prob, skill.patt=skill.patt)
	return(res)
}
		
