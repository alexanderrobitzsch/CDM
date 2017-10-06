## File Name: slca_est_Xlambda.R
## File Version: 0.07
## File Last Change: 2017-10-04 17:59:29


###########################################################################
# estimation of Xlambda parameters
slca_est_Xlambda <- function(Xlambda , Xdes , probs, n.ik1, N.ik1, I, K, G,
	max.increment, TP ,msteps, convM , Xlambda.fixed , XdesM , dimXdes , oldfac,
	decrease.increments, dampening_factor = 1.01, Xlambda.constr.V, e2, V1 )
{	
 	# max.increment0 <- max.increment <- 1
	max.increment0 <- max.increment
	iter <- 1
	eps <- 1e-8
	parchange <- 1
	Xlambda00 <- Xlambda
	Nlam <- length(Xlambda)
	n.ik <- aperm( n.ik1 , c(2,3,1) )
	N.ik <- aperm( N.ik1 , c(2,1) )
	maxK <- K+1
	#--------------------------------
	# begin M-steps
	while( ( iter <= msteps ) & ( parchange > convM)  ){
		Xlambda0 <- Xlambda
		probs <- slca_calc_prob( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda ) 
		d2.b <- d1.b <- rep(eps,Nlam)	
		# probs  num [1:I, 1:maxK, 1:TP]
		# n.ik  num [1:I, 1:maxK, 1:TP]
		# N.ik  num [1:I,1:TP]
		# Xdes  num [1:I, 1:maxK, 1:TP, 1:Nlam] 	    				
		res <- calc_slca_deriv( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda , probs=as.vector(probs) ,
					nik=as.vector(n.ik) , Nik=as.vector(N.ik) )   
		d1.b <- res$d1b
		d2.b <- res$d2b
		increment <- d1.b / ( abs( d2.b + eps ) )
		increment[ is.na(increment) ] <- 0		
		increment <- ifelse(abs(increment)> max.increment, 
					sign(increment)*max.increment , increment )				
		max.increment <- max(abs(increment)) / .98
		Xlambda <- Xlambda + increment
		se.Xlambda <- sqrt( 1 / abs( d2.b+ eps ) )
		if ( ! is.null( Xlambda.fixed) ){
			Xlambda[ Xlambda.fixed[,1] ] <- Xlambda.fixed[,2]
			se.Xlambda[ Xlambda.fixed[,1] ] <- 0		
		}				
		iter <- iter + 1
		parchange <- max( abs(Xlambda0-Xlambda))
	}  # end M-steps
	#-----------------------------------------

		# linear constraints on Xlambda parameters
		# below is code copied from rasch.pml3 (sirt package)
		#................
			# linear constraints: Let e be the vector of error
			# correlations, V a design matrix and c a vector.
			# The constraints can be written in the form 
			# c = V * e . Then V*e - c = 0.
			# See the Neuhaus paper:
			# e_cons = e + V * (V'V)^(-1) * ( c - V * e )
	if ( ! is.null(Xlambda.constr.V) ){
		Xlambda <- slca_est_xlambda_constraint( Xlambda.constr.V=Xlambda.constr.V, V1=V1, e2=e2 ) 
	}				
	
	if (oldfac > 0 ){
		Xlambda <- oldfac*Xlambda00 + ( 1 - oldfac ) *Xlambda
	}
	max.increment <- max( abs( Xlambda - Xlambda00 ))
	if (decrease.increments){ 	
		max.increment0 <- max.increment0 / dampening_factor	
	}					
	#----- output
	res <- list(Xlambda = Xlambda , se.Xlambda = se.Xlambda , max.increment=max.increment0)
	return(res)
}


.slca.est.Xlambda <- slca_est_Xlambda
