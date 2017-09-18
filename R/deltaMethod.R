## File Name: deltaMethod.R
## File Version: 0.03
## File Last Change: 2017-01-31 14:07:26
###############################################################
deltaMethod <- function( derived.pars , est, Sigma , h=1E-5 ){
	#***
	ND <- length(derived.pars)
	#** select h parameters according to size of parameters
	abs_par <- abs(est)	
	hvec <- h * ifelse( abs_par > 1 , abs_par , 1 )
	NP <- length(est)
	#** create design matrix
	M0 <- matrix( est , nrow=1 , ncol=NP)
	M1 <- diag(hvec)	
	
	M1 <- M0[ rep(1,NP) , ] + M1
	M2 <- as.data.frame( rbind( M0 , M1 ) )
	colnames(M2) <- names(est)
	#--- loop over parameters
	A <- matrix( NA , nrow=ND , ncol=NP)
	rownames(A) <- names(derived.pars)
	colnames(A) <- names(est)
	derived.est <- rep( NA , ND)
	names(derived.est) <- names(derived.pars)

	for (dd in 1:ND){
		#dd <- 1
		Md <- stats::model.matrix(derived.pars[[dd]] , M2 )
		if ( ncol(Md) > 1 ){
			Md <- Md[,2]
		}
		A[ dd , ] <- ( Md[-1] - Md[1] ) / hvec
		derived.est[dd] <- Md[1]
	}
	#--- covariance matrix
	derived.Sigma <- A %*% Sigma %*% t(A)
	#--- univariate tests
	se <- sqrt( diag(derived.Sigma) )
	univarTest <- data.frame(
		"parm" = names(derived.pars) ,
		"est" = derived.est , "se" = se ,
		"t" = derived.est / se , 
		"p" = 2 * stats::pnorm( - abs( derived.est / se) )
			)
	rownames(univarTest) <- NULL	
	#--- multivariate test
	R <- diag(ND)
	wt <- WaldTest( delta=derived.est , vcov = derived.Sigma , R = R , nobs = NA)	
	#--- output
	res <- list( 
			"coef" = derived.est , 
			"vcov" = derived.Sigma ,
			"se" = se ,
			"A" = A ,
			"univarTest" = univarTest , 
			"WaldTest" = wt
				)
	return(res)
}
###############################################################
