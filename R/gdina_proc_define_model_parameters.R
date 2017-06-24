
gdina_proc_define_model_parameters <- function( dat.items, q.matrix, rule, HOGDINA, G )
{
	b.attr <- a.attr <- NULL
    I <- nrow(dat.items)   # number of persons
    J <- ncol(dat.items)   # number of items
    K <- ncol(q.matrix)       # number of attributes
    dat.items <- as.matrix( dat.items)
    q.matrix <- as.matrix( q.matrix)            
	if ( length(rule) == 1){ 
		rule <- rep( rule , J )
	}

	if (HOGDINA >= 0){						
		b.attr <- a.attr <- matrix( 0 , nrow=K , ncol=G )
	}	
	#--- OUTPUT
	res <- list(rule=rule, dat.items=dat.items, q.matrix=q.matrix, a.attr=a.attr, b.attr=b.attr,
				I=I, J=J, K=K )
	return(res)
}