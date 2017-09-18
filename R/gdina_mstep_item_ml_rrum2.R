## File Name: gdina_mstep_item_ml_rrum2.R
## File Version: 1.03
## File Last Change: 2017-01-31 14:07:27

#####################################################
# GDINA M-step item parameters
gdina_mstep_item_ml_rrum2 <- function( 
         pjjj , Ilj.ast , Rlj.ast , eps , avoid.zeroprobs , 
		 Mjjj , invM.list , linkfct , rule , method ,
		 iter , delta.new, max.increment , fac.oldxsi,
		 jj , delta , rrum.model, delta.fixed , 
		 mstep_iter , mstep_conv , devchange
			){
			
		eps2 <- eps
		
		delta_jj <- delta[[jj]]
		delta_jj <- stats::qlogis( logpars2rrumpars(delta_jj) )				

		converged <- FALSE
		ii <- 0
		max_increment <- max.increment
		#max_increment <- .35

		eps <- 1E-3
		# sum(Ilj.ast)
		
		Rlj.ast <- Rlj.ast + .005
		Ilj.ast <- Ilj.ast + .05
		
		#*** define function
		ll_FUN <- function(par){
				delta_jj <- par
				delta_jj <- rrumpars2logpars( stats::plogis(delta_jj) )
				irf1 <- ( Mjjj %*% delta_jj )[,1]
				irf1 <- exp(irf1)		
				irf1 <- squeeze.cdm( irf1 , c(eps,1-eps) )				
				# ll <- sum( Rlj.ast * log(irf1) + ( Ilj.ast - Rlj.ast ) * log( 1 - irf1 ) )
				ll <- - sum( Rlj.ast * log(abs(irf1)) + 
						   ( Ilj.ast - Rlj.ast ) * log( abs(1 - irf1 ) ) )
				# ll <- - ll
				return(ll)
						}
		#*** optimzation
		mstep_conv <- 1000		
		res0 <- stats::optim( par = delta_jj , fn=ll_FUN , method="BFGS",
					control=list( maxit = mstep_conv) )
		delta_jj <- res0$par
					
		#*** retransform
		delta_jj <- stats::plogis( delta_jj )
		delta_jj <- rrumpars2logpars( delta_jj )
		delta.new[[jj]] <- delta_jj
		# delta.new[[jj]] <- djj
		if ( (fac.oldxsi > 0 ) & (iter>3)){
		    fac.oldxsi1 <- fac.oldxsi * ( devchange >= 0 )
			delta.new[[jj]] <- fac.oldxsi1*delta[[jj]] + ( 1 - fac.oldxsi1 ) * delta.new[[jj]]
						}

		# fix delta parameter here!!
		if ( ! is.null( delta.fixed ) ){
			delta.fixed.jj <- delta.fixed[[jj]]
			if ( ! is.na( delta.fixed.jj)[1] ){
					delta.new[[jj]] <- delta.fixed.jj
									}
							}						
		#*** output	
		res <- list( 
				delta.new = delta.new 
					)	
		return(res)	
												
			}
######################################################			
			
			
