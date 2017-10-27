## File Name: slca_proc_multiple_groups.R
## File Version: 0.02

slca_proc_multiple_groups <- function( group, n)
{
	if ( is.null(group)){ 
		G <- 1 
		group0 <- group <- rep(1,n)
	} else {
		group0 <- group
		gr2 <- cdm_sort_unique(x=group)
		G <- length(gr2)
		group <- match( group , gr2 )
	}
	group.stat <- NULL
	if (G>1){
		a1 <- stats::aggregate( 1+0*group , list(group) , sum )
		a2 <- rep("",G)
		for (gg in 1:G){
			a2[gg] <- group0[ which( group == gg )[1]  ]
		}
		group.stat <- cbind( a2 , a1 )
		colnames(group.stat) <- c(  "group.orig" , "group" , "N"  )
	    Ngroup <- a1[,2]		
	}	
    if (G==1){ 
		Ngroup <- length(group) 
	}
	#---- output
	res <- list(G=G, group=group, group0=group0, group.stat=group.stat, Ngroup=Ngroup)
	return(res)
}
