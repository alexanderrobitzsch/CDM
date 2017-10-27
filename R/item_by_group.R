## File Name: item_by_group.R
## File Version: 0.12

##########################################################
# creates an extended dataset with item responses in which
# items are defined as combinations of original items and
# group
item_by_group <- function( dat , group , invariant = NULL ,
		rm.empty = TRUE )
{
	vars <- colnames(dat)
	some_invariant_items <- ( ! is.null(invariant) )	
	if ( some_invariant_items ){
		vars <- setdiff(vars,invariant)
	}
	
	I <- length(vars)
	group_unique <- sort( unique(group) )
	G <- length(group_unique)
	#*** create extended dataset
	dat2 <- matrix( NA , nrow = nrow(dat) , ncol= I*G )
	cn <- sapply( vars , FUN = function(vv){
			  paste0( vv , "_group" , group_unique ) } , simplify=FALSE)
	colnames(dat2) <- unlist(cn)
	for (gg in 1:G){
		# gg <- 1
		ind_gg <- which( group == group_unique[gg] )	
		for (ii in 1:I){
			# ii <- 1
			dat2[ ind_gg , G*(ii-1) + gg ] <- dat[ ind_gg , vars[ii] ]
		}	
	}
	#--- include invariant items
	if ( some_invariant_items ){
		dat2a <- dat[ , invariant]
		dat2 <- cbind( dat2a , dat2 )
	}	
	
	#--- remove empty columns
	if (rm.empty){
		ind <- which( colMeans( is.na(dat2) ) == 1 )
		if ( length(ind) > 0 ){
			dat2 <- dat2[ , - ind ]
		}
	}
	#--- include some attributes: variables and variable indices
	attr(dat2,"noninvariant") <- vars
	attr(dat2,"invariant") <- invariant
	attr(dat2,"noninvariant_index") <- match( vars , colnames(dat))
	attr(dat2,"noninvariant_index_extended") <- 
			rep( attr(dat2,"noninvariant_index") , each = G )
	attr(dat2,"invariant_index") <- match( invariant , colnames(dat))	
	attr(dat2,"all_index") <- c( attr(dat2,"invariant_index") , 
					attr(dat2,"noninvariant_index_extended") )
	cn <- colnames(dat2)
	names(cn) <- NULL
	colnames(dat2) <- cn					
	#--- output
	return(dat2)
}
############################################################
