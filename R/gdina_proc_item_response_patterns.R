
gdina_proc_item_response_patterns <- function( dat.items, J, G, weights, group )
{
    # string with item response patterns
    item.patt.subj <- dat.items[,1]
	for (jj in 2:J){
		item.patt.subj <- paste( item.patt.subj , dat.items[,jj] , sep="")
	}	
    # calculate frequency of each item response pattern
    item.patt <- table( item.patt.subj )
	
    # sort item response pattern according to their absolute frequencies
    six <- sort( item.patt, index.return=FALSE, decreasing=TRUE)
    # define data frame 'item.patt' with item response pattern and its frequency (weight)
    item.patt <- cbind( "pattern" = rownames(six), "freq" = as.numeric(as.vector(six) ) )
	
    # calculate weighted frequency for each item response pattern
	if (G== 1){ 
		h1 <- rowsum( weights , item.patt.subj )	
		item.patt[,2] <- h1[ match( item.patt[,1] , rownames(h1) ) , 1]							
		item.patt.freq <- as.numeric(item.patt[,2])
	}			
				
	if (G > 1){
		item.patt.freq <- matrix( 0 , nrow(item.patt) , G )
		for (gg in 1:G){  
			h1 <- rowsum( weights * (group == gg ), item.patt.subj )	
			item.patt[,2] <- h1[ match( item.patt[,1] , rownames(h1) ) , 1]							
			item.patt.freq[,gg] <- as.numeric(item.patt[,2])
		}
	}
	#---- OUTPUT
	res <- list(item.patt.subj=item.patt.subj, item.patt=item.patt, six=six,
					item.patt.freq=item.patt.freq)
	return(res)
}