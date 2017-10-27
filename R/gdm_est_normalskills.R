## File Name: gdm_est_normalskills.R
## File Version: 0.08

##############################################################
# estimation of skill distribution under normality
gdm_est_normalskills <- function( pi.k , theta.k , irtmodel, G , D ,
	mean.constraint , Sigma.constraint , standardized.latent ,
	p.aj.xi , group , ind.group , weights , b , a )
{
	# mean.constraint [ dimension , group , value ]
	# Sigma.constraint [ dimension1 , dimension2 , group , value ]	
   
   #-----------------------------------------
   #-------- unidimensional model -----------
   #-----------------------------------------   
   if (D==1){
		for (gg in 1:G){
			res <- cdm_fit_normal(x=theta.k, w=pi.k[,gg])
			mg <- res$Mu
			sdg <- sqrt(res$Sigma)			
			#--------- mean constraint
			if ( ( ! is.null ( mean.constraint )) ){
				i1 <- mean.constraint[ mean.constraint[,2] == gg , , drop=FALSE]	
				if ( ( nrow(i1) == 1 ) & (G>1) ){ 	
					if ( ( gg==1 ) & (i1[,1]==1) & (i1[,2]==1) ){ 
						b <- b + ( mg - i1[3] ) 
					}
					mg <- i1[3] 
				}
				if ( nrow(i1) > 0 ){				
					mg <- i1[,3]				
				}						
			}	# end mean constraint
			#--------- sigma constraint
			if ( ( ! is.null ( Sigma.constraint ) )  ){
				i1 <- Sigma.constraint[ Sigma.constraint[,3] == gg , , drop=FALSE]
				if ( ( nrow(i1) == 1 ) & (G>1) ){ 		  
					if ( ( gg==1 ) & (i1[,1]==1) & (i1[,2]==1) ){ 
						a <- a * sdg / sqrt(i1[4])  
					}		
					sdg <- sqrt(i1[4]) 
				}
			}  # end sigma constraint		
		pi.k[,gg] <- cdm_sumnorm( stats::dnorm( theta.k[,1] ,mean=mg , sd=sdg)	)
		}			
	}
   #-----------------------------------------
   #-------- multidimensional model ---------
   #-----------------------------------------
   if (D>1){
		for (gg in 1:G){
			res <- cdm_fit_normal(x=theta.k, w=pi.k[,gg] )
			mean.gg <- res$Mu
			Sigma.gg <- res$Sigma
			Sigma.gg <- cdm_add_ridge_diagonal(x=Sigma.gg , eps=1E-10 )
			#----- mu constraint
			m.gg <- mean.constraint[ mean.constraint[,2] == 1 , ]
			if ( ! is.null(mean.constraint)){
				if( dim(m.gg)[1] > 0 ){
					mean.gg[ m.gg[,1] ] <- m.gg[,3]
				}
			}
			s.gg <- Sigma.constraint[ Sigma.constraint[,3] == 1 , ]			
			#----- sigma constraint
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
			pi.k[,gg] <- cdm_sumnorm( mvtnorm::dmvnorm( theta.k , mean=mean.gg , sigma = Sigma.gg )	)
		}  # end gg
	}  # end multidimensional model
	#--- OUTPUT
	res <- list(pi.k=pi.k , b = b , a=a )
	return(res)
}
#*************************************************************

.gdm.est.normalskills <- gdm_est_normalskills
