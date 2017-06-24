
gdina_post_pattern_output <- function(G, p.xi.aj, zeroprob.skillclasses,
	item.patt, attr.patt.c, p.aj.xi, item.patt.subj, group2, attr.patt, K )
{

    # calculate posterior probability for each attribute pattern
	if (G==1){	
		# set likelihood for skill classes with zero probability to zero
		if ( ! is.null(zeroprob.skillclasses) ){
			p.xi.aj[ , zeroprob.skillclasses ] <- 0
		}
		pattern <- data.frame( 
						freq = round(as.numeric(item.patt[,-1]),3),
						mle.est = attr.patt.c[ max.col( p.xi.aj ) ], 
						mle.post = rowMaxs( p.xi.aj ) / rowSums( p.xi.aj ), 
						map.est = attr.patt.c[ max.col( p.aj.xi ) ], 
						map.post = rowMaxs( p.aj.xi ) )
	}				
	if (G>1){
		ind1 <- match( item.patt.subj , item.patt[,1] )
		l1 <- attr.patt.c[ max.col( p.xi.aj ) ]
		pattern <- data.frame( "mle.est" = l1[ind1] ) 
		l1 <- rowMaxs( p.xi.aj ) / rowSums( p.xi.aj )
		pattern$mle.post <- l1[ind1]
		pattern$map.est <- NA
		pattern$map.post <- NA
		for (gg in 1:G){
			# gg <- 1	
			ind.gg <- which( group2 == gg )
			ind2.gg <- match( item.patt.subj[ind.gg] , item.patt[  , 1] )
			l1 <- attr.patt.c[ max.col( p.aj.xi[,,gg] ) ]
			pattern$map.est[ind.gg] <- l1[ind2.gg]
			l1 <-  rowMaxs( p.aj.xi[,,gg] )
			pattern$map.post[ind.gg] <- l1[ind2.gg]		
					}
			}
    # calculate posterior probabilities for all skills separately
	if (G==1){
		attr.postprob <- p.aj.xi %*% attr.patt
		colnames( attr.postprob ) <- paste("post.attr",1:K, sep="")
		pattern <- cbind( pattern,  attr.postprob )
	}
	#--------- OUTPUT
	res <- list( pattern = pattern, p.xi.aj = p.xi.aj )
	return(res)
}