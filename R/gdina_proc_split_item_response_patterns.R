## File Name: gdina_proc_split_item_response_patterns.R
## File Version: 0.01
## File Last Change: 2017-06-05 14:12:17

gdina_proc_split_item_response_patterns <- function( item.patt, J )
{
    # split item response pattern in a data frame with items as columns
    spl <- sapply( as.vector(item.patt[,1]), FUN = function(ii){ strsplit( ii, split = NULL) } ) 
    item.patt.split <- matrix( rep( 0, length(spl) * J ), ncol=J )
    for (ll in 1:length(spl) ){
        item.patt.split[ ll, ] <- as.numeric( spl[[ll]] )
    }
    # response pattern matrix: each observed entry corresponds to a 1, each unobserved entry to a 0
    resp.patt <- 1* ( item.patt.split != 9 )
    # number of item response patterns
    IP <- nrow(item.patt.split)  
	#---- OUTPUT
	res <- list(IP=IP, resp.patt=resp.patt, item.patt.split=item.patt.split)
	return(res)
}
