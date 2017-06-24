


##############################################################
# estimation of skill distribution under normality
gdm_est_normalskills <- function( pi.k , theta.k , irtmodel,G , D ,
	mean.constraint , Sigma.constraint , standardized.latent ,
	p.aj.xi , group , ind.group , weights , b , a )
{
	# mean.constraint [ dimension , group , value ]
	# Sigma.constraint [ dimension1 , dimension2 , group , value ]	
   ####################################
   # unidimensional model
   if (D==1){
	for (gg in 1:G){
		mg <- sum( theta.k[,1] * pi.k[,gg] )
		sdg <- sqrt( sum( theta.k[,1]^2 * pi.k[,gg] ) - mg^2 )
	if ( (! is.null ( mean.constraint ))  ){
		i1 <- mean.constraint[ mean.constraint[,2] == gg , , drop=FALSE]	
#		  if ( ( nrow(i1) == 1 ) & (G>=1) ){ 	
		  if ( ( nrow(i1) == 1 ) & (G>1) ){ 	
				if ( ( gg==1 ) & (i1[,1]==1) & (i1[,2]==1) ){ 
					b <- b + ( mg - i1[3] ) 
						}
				mg <- i1[3] 
						}
		  if ( nrow(i1) > 0 ){				
				mg <- i1[,3]				
							}						
					}				
	if ( ( ! is.null ( Sigma.constraint ) )  ){
		i1 <- Sigma.constraint[ Sigma.constraint[,3] == gg , , drop=FALSE]
#		  if ( ( nrow(i1) == 1 ) & (G>=1) ){ 	
		  if ( ( nrow(i1) == 1 ) & (G>1) ){ 		  
			if ( ( gg==1 ) & (i1[,1]==1) & (i1[,2]==1) ){ 
	                a <- a * sdg / sqrt(i1[4])  
							}		
				sdg <- sqrt(i1[4]) 
						}
					}		
		pi.k[,gg] <- stats::dnorm( theta.k[,1] ,mean=mg , sd=sdg)		
		pi.k[,gg] <- pi.k[,gg] / sum( pi.k[,gg] )
		
			}			
		}
	#####################################
    # multidimensional model	
	if (D>1){
	  for (gg in 1:G){
		# gg <- 1
		mean.gg <- rep(0,D)
		Sigma.gg <- diag(0,D)
		
		for (dd in 1:D){
			# dd <- 1
			mean.gg[dd] <- sum( pi.k[,gg] * theta.k[,dd] )
				}
		for (dd1 in 1:D){
			for (dd2 in dd1:D){
#		dd1 <- 1 ; 	dd2 <- 1
		Sigma.gg[dd1,dd2] <- sum( pi.k[,gg] * (theta.k[,dd1] - mean.gg[dd1] )*(theta.k[,dd2] - mean.gg[dd2] ) ) 
#		Sigma.gg[dd1,dd2] <- Sigma.gg[dd1,dd2] - mean.gg[dd1] * mean.gg[dd2]
		Sigma.gg[dd2,dd1] <- Sigma.gg[dd1,dd2]
							}
						}
		Sigma.gg <- Sigma.gg + diag(10^(-10) , D )
		m.gg <- mean.constraint[ mean.constraint[,2] == 1 , ]
		if ( ! is.null(mean.constraint)){
		if( dim(m.gg)[1] > 0 ){
			mean.gg[ m.gg[,1] ] <- m.gg[,3]
								}}
		s.gg <- Sigma.constraint[ Sigma.constraint[,3] == 1 , ]	
		
#		if ( standardized.latent & ( gg == 1 )){
#			Sigma.gg <- cov2cor( Sigma.gg )
#				}
		
		
		if ( ! is.null(Sigma.constraint)){
		if( dim(s.gg)[1] > 0 ){		
			c1 <- stats::cov2cor( Sigma.gg )
				d1 <- diag(Sigma.gg)
				s.gg1 <- s.gg[ s.gg[,1] == s.gg[,2] , ]
				if ( nrow(s.gg1) > 0 ){
					d1[ s.gg1[,1:2] ] <- s.gg[,4]
						}
				d1 <- outer( sqrt(d1) , sqrt(d1) )*c1			
				s.gg2 <- s.gg[ s.gg[,1] != s.gg[,2] , ]
				if ( nrow(s.gg1) > 0 ){
					d1[ s.gg1[,1:2] ] <- s.gg[,4]
					d1[ s.gg1[,c(2,1)] ] <- s.gg[,4]					
						}
			Sigma.gg <- d1
						}
								}
		pi.k[,gg] <- mvtnorm::dmvnorm( theta.k , mean=mean.gg , sigma = Sigma.gg )			
		pi.k[,gg] <- pi.k[,gg] / sum( pi.k[,gg] )					
					}
				}
	#--- OUTPUT
	res <- list("pi.k"=pi.k , "b" = b , "a"=a )
	return(res)
}
#*************************************************************

.gdm.est.normalskills <- gdm_est_normalskills