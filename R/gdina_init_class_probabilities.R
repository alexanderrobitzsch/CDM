## File Name: gdina_init_class_probabilities.R
## File Version: 0.01

gdina_init_class_probabilities <- function( G, L, seed , attr.prob.init )
{
	if (G== 1){ 
		attr.prob <- rep( 1/L, L )
		if ( seed > 0 ){
		   set.seed(seed)
		   attr.prob <- stats::runif( L , 0 , 10/L )
		   attr.prob <- attr.prob / sum( attr.prob )
		}		   
	} else {
		attr.prob <- matrix( 1/L , L , G )
		if ( seed > 0 ){
		   set.seed(seed)
		   for (gg in 1:G){
			   a1 <- runif( L , 0 , 10/L )
			   attr.prob[,gg] <- a1 / sum( a1 )
			}
		}			
	}
	#--- initial class probabilities					
	if ( ! is.null(attr.prob.init) ){
        attr.prob <- attr.prob.init
		if (G==1){ 
			attr.prob <- as.vector( attr.prob) 
		}
	}
	#---- OUTPUT
	res <- list(attr.prob=attr.prob)
	return(res)
}
