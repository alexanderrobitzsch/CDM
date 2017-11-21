## File Name: gdm_calc_ic.R
## File Version: 0.05


#############################################################
# calculation of information criteria and number of parameters
gdm_calc_ic <- function( dev , dat , G ,  skillspace , irtmodel , 
			K,D,TD,I,b.constraint,a.constraint , mean.constraint ,
			Sigma.constraint , delta.designmatrix , standardized.latent ,
			data0 , centerslopes , TP , centerintercepts, centered.latent )
{
	ic <- list( "deviance" = dev , "n" = nrow(data0) )
	ic$traitpars <- 0
	ic$itempars <- 0	
	#******
	# Until now this works in one dimension
	# trait parameters: normal skillspace
	if ( skillspace == "normal" ){
		if (irtmodel=="1PL" & ( D==1 )){
			ic$traitpars <- 2*(G-1)	+ 1
		}
		if ( ( irtmodel %in% c("2PL","2PLcat") ) & (D==1) ){
			ic$traitpars <- 2*(G-1)
			if (!standardized.latent){
				ic$traitpars <- ic$traitpars + 2
			}
		}	
		if (D > 1 ){
			ic$traitpars <- 2 * D*G + D*(D-1)/2*G
			if ( ! is.null(mean.constraint) ){ 
					ic$traitpars <- ic$traitpars - nrow(mean.constraint)			
			}
			if ( ! is.null(Sigma.constraint) ){ 
					ic$traitpars <- ic$traitpars - nrow(Sigma.constraint)			
			}
		}
	}	# end normal

	#******
	# trait parameters: loglinear skillspace
	if ( skillspace == "loglinear" ){
		ic$traitpars <- G*(ncol(delta.designmatrix) - 1)
	}
	if ( skillspace == "full" ){
		ic$traitpars <- G*(TP-1)
	}	
	if ( skillspace == "est" ){
		ic$traitpars <- G*(TP-1) + TP*TD
	}							
	#************************************************
	# item parameters b
	ic$itempars.b <- I*K
	if ( ! is.null(b.constraint)){
		ic$itempars.b <- ic$itempars.b - nrow(b.constraint)
	}	

	#************************************************
	# item parameters a
	ic$itempars.a <- 0
	if ( irtmodel == "2PL"){ 
		ic$itempars.a <- I*TD
		if ( ! is.null(a.constraint)){
			a.constraint2 <- a.constraint[ a.constraint[,3] == 1 , , drop=FALSE]
			ic$itempars.a <- ic$itempars.a - nrow(a.constraint2)
		}	
	}
	ic$centeredintercepts <- (centerintercepts)*D						
	ic$centeredslopes <- (centerslopes)*D
	if ( irtmodel == "2PLcat"){ 
		ic$itempars.a <- I*TD*K
		if ( ! is.null(a.constraint)){
			ic$itempars.a <- ic$itempars.a - nrow(a.constraint)
		}	
	}
	#***********************************************
	# information criteria
	ic$itempars <- ic$itempars.a + ic$itempars.b - ic$centeredintercepts - ic$centeredslopes
	ic$np <- ic$itempars + ic$traitpars	
	
	# AIC
	ic$AIC <- dev + 2*ic$np
	# BIC
	ic$BIC <- dev + ( log(ic$n) )*ic$np
	# CAIC (conistent AIC)
	ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
	# corrected AIC
	ic$AICc <- ic$AIC + 2*ic$np * ( ic$np + 1 ) / ( ic$n - ic$np - 1 )				
	return(ic)
}
###################################################################
		
.gdm.calc.ic <- gdm_calc_ic
