## File Name: gdina_post_posterior_output.R
## File Version: 0.02
## File Last Change: 2017-06-17 14:09:55

gdina_post_posterior_output <- function(G, p.aj.xi, p.xi.aj, pattern, data, item.patt.subj,
		item.patt, attr.prob, group)
{
	
	if (G==1){	
		rownames( p.aj.xi ) <- rownames( pattern ) # output rownames posterior probabilities    
		pattern <- data.frame(pattern) # convert pattern to numeric format
		for (vv in seq(1,ncol(pattern))[ -c(2,4) ] ){
						pattern[,vv ] <- as.numeric( paste( pattern[,vv] ) ) }
		
		# subject pattern
		item.patt.subj <- data.frame( "case" = 1:(nrow(data) ), 
									   "pattern" = item.patt.subj, 
                                       "pattern.index" = match( item.patt.subj, item.patt[,1] ) )											
		# attribute pattern (expected frequencies)
		attr.prob0 <- attr.prob
		attr.prob <- data.frame( attr.prob )
		attr.prob$class.expfreq <-  attr.prob[,1] * nrow(data) 
		
		#*****
		pattern <- pattern[ item.patt.subj$pattern.index , ]	
		pattern[,1] <- paste( item.patt.subj$pattern )
		colnames(pattern)[1] <- "pattern"
		p.aj.xi <- p.aj.xi[ item.patt.subj$pattern.index , ]
		rownames(p.aj.xi) <- pattern$pattern
		p.xi.aj <- p.xi.aj[ item.patt.subj$pattern.index , ]
		rownames(p.xi.aj) <- pattern$pattern
	}
	
	#-------
	
	if (G==1){
		posterior <- p.aj.xi
	}
	if (G>1){
		ind <- match( item.patt.subj , item.patt[,1] )			
		p.xi.aj <- p.xi.aj[ ind , ]
		rownames(p.xi.aj) <- pattern$pattern
		p.aj.xi <- p.aj.xi[ ind , , ]
		rownames(p.aj.xi) <- pattern$pattern				
		ND <- dim(p.aj.xi)
		posterior <- matrix( 0 , nrow=ND[1] , ncol=ND[2] )
	    for (gg in 1:G){
			ind.gg <- which( group == gg )
			posterior[ ind.gg , ] <- p.aj.xi[ ind.gg , , gg ]
		}
		attr.prob0 <- attr.prob											
	}
	# labels likelihood
	colnames(p.xi.aj) <- paste(rownames(attr.prob))	
	
	#--------- OUTPUT
	res <- list( item.patt.subj=item.patt.subj, attr.prob=attr.prob, p.xi.aj=p.xi.aj, posterior=posterior,
					pattern=pattern, attr.prob0 = attr.prob0 )
	return(res)
}
