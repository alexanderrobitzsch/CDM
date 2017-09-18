## File Name: gdina_init_item_parameters.R
## File Version: 0.02
## File Last Change: 2017-06-05 13:18:08

gdina_init_item_parameters <- function( delta.init, linkfct, J, seed, Mj,
	delta.basispar.init, delta.designmatrix, Mj.index, rule )
{
	delta <- delta.init
	if ( is.null( delta.init ) ){		
		#**** identity link
		if (linkfct == "identity" ){ 	
			for (jj in 1:J){
				N1jj <- ncol(Mj[[jj]][[1]])
				l1 <- rep(0,N1jj)
				if ( seed == 0 ){
					dd1 <- .2 ; dd2 <- .6 
				} else {
					dd1 <- stats::runif( 1 , 0 , .4 )
					dd2 <- stats::runif( 1 , 0 , 1 - dd1 - .1 )				
				}
				l1[1] <- dd1
				l1[2:N1jj] <- rep( dd2 / (N1jj - 1) , N1jj - 1 )
				delta[[jj]] <- l1
			}
		}
		#**** logit link
		if (linkfct == "logit" ){ 	
			for ( jj in 1:J){
				N1jj <- ncol(Mj[[jj]][[1]])
				l1 <- rep(0,N1jj)
				if ( seed == 0 ){
					dd1 <- -1 ; dd2 <- 1
				} else {			
					dd1 <- stats::runif( 1 , -2 , 0 )					
					dd2 <- stats::runif( 1 , 0 , 2 )					
				}
				l1[1] <- dd1
				l1[N1jj] <- dd2
				delta[[jj]] <- l1
			}
		}
		#**** log link
		if (linkfct == "log" ){ 	
			for ( jj in 1:J){
				N1jj <- ncol(Mj[[jj]][[1]])
				l1 <- rep(0,N1jj)
				if ( seed == 0 ){
					dd1 <- -1.5 ; dd2 <- .75
				} else {
					dd1 <- stats::runif( 1 , -3 , -1 )					
					dd2 <- stats::runif( 1 , .25 , 1 )
				}
				l1[1] <- dd1
				l1[N1jj] <- dd2									
				if ( rule[jj] == "ACDM"){
					v1 <- .5 + 0*l1
					v1[1] <- .80
					l1 <- rrumpars2logpars(v1) 														
				}						
				delta[[jj]] <- l1
			}
		}	
	}
	###########################
	# import inits delta basis parameter
	if ( ! is.null( delta.basispar.init ) ){
		u.delta <- delta.designmatrix %*% delta.basispar.init
		for (jj in 1:J){
			delta[[jj]] <- u.delta[ seq( Mj.index[jj,2] , Mj.index[jj,3] ) , 1]
		}
	}			
	#--- OUTPUT
	return(delta)
}
