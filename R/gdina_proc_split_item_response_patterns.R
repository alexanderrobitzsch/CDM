## File Name: gdina_proc_split_item_response_patterns.R
## File Version: 0.04

gdina_proc_split_item_response_patterns <- function( item.patt, J, freq_weights = TRUE,
		resp=NULL, dat.items = NULL  )
{
	#---------- use frequency weights
	if (freq_weights ){
		# split item response pattern in a data frame with items as columns
		spl <- sapply( as.vector(item.patt[,1]), FUN = function(ii){ strsplit( ii, split = NULL) } ) 
		item.patt.split <- matrix( rep( 0, length(spl) * J ), ncol=J )
		for (ll in 1:length(spl) ){
			item.patt.split[ ll, ] <- as.numeric( spl[[ll]] )
		}
		# response pattern matrix: each observed entry corresponds to a 1, 
		# each unobserved entry to a 0
		resp.patt <- 1* ( item.patt.split != 9 )
	}
	#---------- do not use frequency weights
	if ( ! freq_weights ){
		item.patt.split <- dat.items
		resp.patt <- resp
	}
	# number of item response patterns
	IP <- nrow(item.patt.split)  
	#---- OUTPUT
	res <- list(IP=IP, resp.patt=resp.patt, item.patt.split=item.patt.split)
	return(res)
}
