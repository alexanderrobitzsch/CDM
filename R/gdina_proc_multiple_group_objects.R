## File Name: gdina_proc_multiple_group_objects.R
## File Version: 0.04

gdina_proc_multiple_group_objects <- function(group)
{
	G <- 1
	group0 <- group
	groupre <- FALSE
	group2 <- NULL
	if ( is.factor( group ) ){ 
		group <- paste( group ) 
	}
	if ( ! is.null( group) ){
		group0 <- group
		groups <- sort( unique( group) )
		G <- length(groups)	
		group2 <- match( group , groups )
		if ( any( group != group2 ) ){
			group <- group2
			groupre <- TRUE
		}
	}
	group.stat <- NULL
	if ( G > 1 ){
		# group statistics
		a1 <- stats::aggregate( 1+0*group , list(group) , sum )
		a2 <- rep("",G)
		for (gg in 1:G){
			a2[gg] <- group0[ which( group == gg )[1]  ]
		}
		group.stat <- cbind( a2 , a1 )
		colnames(group.stat) <- c(  "group.orig" , "group" , "N"  )	
	}
	
	#---- OUTPUT
	res <- list(G=G, group=group, group0=group0, groupre=groupre, group.stat=group.stat,
					group2=group2)
	return(res)
}
