## File Name: slca.R
## File Version: 1.839


###########################################
# Structured latent class analysis
###########################################

slca <- function( data , group=NULL, 
    weights=rep(1, nrow(data)), 
	Xdes ,  Xlambda.init=NULL , Xlambda.fixed=NULL , 
	Xlambda.constr.V=NULL , Xlambda.constr.c=NULL , 
	delta.designmatrix =NULL ,  delta.init = NULL , 
	delta.fixed = NULL , delta.linkfct = "log" ,  
	Xlambda_positive = NULL, 
	regular_lam = 0 , regular_w = NULL , regular_n = nrow(data) , 
    maxiter=1000, conv=1E-5, globconv=1E-5, msteps=10 , 
	convM=.0005 , decrease.increments = FALSE , oldfac = 0 , dampening_factor=1.01,
	seed=NULL , progress = TRUE , PEM=TRUE, PEM_itermax=maxiter, ...)
{	
	#************************************************************
	# mean.constraint [ dimension , group , value ]
	# Sigma.constraint [ dimension1 , dimension2 , group , value ]	
	cl <- match.call()
    theta.k <- NULL	
	#*************************
	# data preparation
	s1 <- Sys.time()
	e1 <- environment()	
	use.freqpatt <- FALSE 
	deviance.history <- rep(NA, maxiter)	
	
	## prevent from warnings in R CMD check "no visible binding"
	## gdm: no visible binding for global variable 'TD'
	TD <- TP <- EAP.rel <- mean.trait <- sd.trait <- skewness.trait <- NULL
	K.item <- correlation.trait <- NULL 
    se.theta.k <- NULL
	
	#-- data processing
	res <- slca_proc_data(data=data)
	dat <- res$dat
	dat.ind <- res$dat.ind
	I <- res$I
	n <- res$n
	dat.resp <- res$dat.resp
	K <- res$K
	maxK <- res$maxK
	data <- res$data
	data0 <- res$data0
	resp.ind.list <- res$resp.ind.list	

	#-- process data for multiple groups
	res <- slca_proc_multiple_groups( group=group, n=n ) 
	G <- res$G
	group <- res$group
	group0 <- res$group0
	group.stat <- res$group.stat
	Ngroup <- res$Ngroup
	
	#--- define design matrices
	KK <- K	# if KK == 1 then a slope parameter for all items is estimated
    deltaNULL <- 0
	if ( is.null(delta.designmatrix) ){
	    deltaNULL <- 1
		delta.designmatrix <- diag( dim(Xdes)[3] )
	}

	# lambda basis parameters for X
	
	#--- set seed
	res <- slca_set_seed(seed=seed)	
	seed.used <- res$seed.used
	
	#--- inits Xlambda
	Nlam <- dim(Xdes)[[4]]
    res <- slca_inits_Xlambda( Xlambda.init=Xlambda.init, Xdes=Xdes, Nlam=Nlam, Xlambda_positive=Xlambda_positive, 
				Xlambda.fixed=Xlambda.fixed ) 
	Xlambda <- Xlambda.init <- res$Xlambda.init
	Xlambda_positive <- res$Xlambda_positive									
	
	#-- starting values for distributions	
	res <- slca_inits_skill_distribution( delta.designmatrix=delta.designmatrix, delta.init=delta.init, 
				delta.linkfct=delta.linkfct, G=G, K=K, I=I ) 
	TP <- res$TP
	n.ik <- res$n.ik
	pi.k <- res$pi.k
	delta <- res$delta

	se.Xlambda <- 0*Xlambda
	max.increment.Xlambda <- .3

	#--- preparations for calc.counts
	res <- gdm_prep_calc_counts( K=K, G=G, group=group, weights=weights, dat.resp=dat.resp, dat.ind=dat.ind, 
				use.freqpatt=use.freqpatt ) 
	ind.group <- res$ind.group
	dat.ind2 <- res$dat.ind2				
	
	#--- reducing computational burden for design matrix
	res <- slca_proc_design_matrix_xlambda(Xdes=Xdes)	
	XdesM <- res$XdesM
	NX <- res$NX
	dimXdes <- res$dimXdes
	
	#-- Xlambda constraints	
	V <- e2 <- V1 <- NULL
	if ( ! is.null(Xlambda.constr.V) ){
		V <- Xlambda.constr.V
		e2 <- matrix( Xlambda.constr.c , nrow=ncol(V) , ncol=1 )
		V1 <- solve( crossprod(V) )
	}				

	#-- regularization	
	res <- slca_proc_regularization( regular_lam=regular_lam, regular_w=regular_w, Nlam=Nlam, Xlambda.fixed=Xlambda.fixed, 
					regular_n=regular_n ) 
	regular_lam <- res$regular_lam
	regular_w <- res$regular_w
	regular_lam_used <- res$regular_lam_used
	regular_indicator_parameters <- res$regular_indicator_parameters
	regularization <- res$regularization
	
	#-- preliminaries PEM acceleration
	if (PEM){	
		envir <- environment()	
		pem_pars <- c("delta","Xlambda")				
		pem_output_vars <- c("pi.k","Xlambda","delta")
		parmlist <- cdm_pem_inits_assign_parmlist(pem_pars=pem_pars, envir=envir)
		res <- cdm_pem_inits( parmlist=parmlist)
		pem_parameter_index <- res$pem_parameter_index
		pem_parameter_sequence <- res$pem_parameter_sequence				
		if (regularization){
			PEM <- FALSE
		}
	}

	#- for posterior calculation
	gwt0 <- matrix( 1 , nrow=n , ncol=TP )	
					
	#---
	# initial values algorithm
	max.increment <- 1
	dev <- 0	; iter <- 0
	globconv1 <- conv1 <- 1000
	disp <- paste( paste( rep(".", 70 ) , collapse="") ,"\n", sep="")
	mindev <- Inf
	iterate <- TRUE
	
	############################################
	# BEGIN MML Algorithm
	############################################
		
	while( ( iter < maxiter ) & ( ( globconv1 > globconv) | ( conv1 > conv) ) & iterate ){
		
		#--- collect old parameters
		Xlambda0 <- Xlambda 
		dev0 <- dev
		delta0 <- delta
		pi.k0 <- pi.k
 		
		#--- 1 calculate probabilities
		probs <- slca_calc_prob( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda ) 

		#--- 2 calculate individual likelihood
		res.hwt <- slca_calc_posterior( probs=probs, gwt0=gwt0, dat=dat, I=I, resp.ind.list=resp.ind.list )
		p.xi.aj <- res.hwt$hwt 	
		
		#--- 3 calculate posterior and marginal distributions
		res <- gdm_calc_post( pi.k=pi.k, group=group, p.xi.aj=p.xi.aj, weights=weights, G=G, ind.group=ind.group, 
					use.freqpatt=use.freqpatt ) 
		p.aj.xi <- res$p.aj.xi
		pi.k <- res$pi.k
		
		#*****
		#4 calculate expected counts
		# n.ik [ 1:TP , 1:I , 1:(K+1) , 1:G ]
		res <- slca_calc_counts( G=G, weights=weights, dat.ind=dat.ind, dat=dat, dat.resp=dat.resp, p.aj.xi=p.aj.xi, K=K, 
					n.ik=n.ik, TP=TP, I=I, group=group, dat.ind2=dat.ind2, ind.group=ind.group, 
					use.freqpatt=use.freqpatt ) 
		n.ik <- res$n.ik
		n.ik1 <- res$n.ik1
		N.ik <- res$N.ik
		N.ik1 <- res$N.ik1

		#*****
		#5 M step: Xdelta parameter estimation
		# n.ik  [1:TP,1:I,1:K,1:G]
		# probs[1:I,1:K,1:TP]
        res <- slca_est_Xlambda( Xlambda=Xlambda, Xdes=Xdes, probs=probs, n.ik1=n.ik1, N.ik1=N.ik1, I=I, K=K, G=G, 
					 max.increment=max.increment, TP=TP, msteps=msteps, convM=convM, 
					 Xlambda.fixed=Xlambda.fixed, XdesM=XdesM, dimXdes=dimXdes, oldfac=oldfac, 
					 decrease.increments=decrease.increments, dampening_factor=dampening_factor, 
					 Xlambda.constr.V=Xlambda.constr.V, e2=e2, V1=V1, regularization=regularization, 
					 regular_lam_used=regular_lam_used, regular_n=regular_n, Xlambda_positive=Xlambda_positive ) 
		Xlambda <- res$Xlambda
		se.Xlambda <- res$se.Xlambda
		max.increment <- res$max.increment	
		regular_penalty <- res$regular_penalty
					
		#*****
		#7 M step: estimate reduced skillspace
		res <- slca_est_skillspace( Ngroup=Ngroup, pi.k=pi.k, delta.designmatrix=delta.designmatrix, G=G, delta=delta, 
					delta.fixed=delta.fixed, eps=1E-7, oldfac=oldfac, delta.linkfct=delta.linkfct ) 
		pi.k <- res$pi.k
		delta <- res$delta
		covdelta <- res$covdelta

		#******
		#7a P-EM acceleration

		#-- PEM acceleration
		if (PEM){
			#-- collect all parameters in a list
			parmlist <- cdm_pem_inits_assign_parmlist(pem_pars=pem_pars, envir=envir)			
			#-- define log-likelihood function
			ll_fct <- slca_calc_loglikelihood
			#- extract parameters
			ll_args <- list( Xlambda=Xlambda, delta=delta, delta.designmatrix=delta.designmatrix, XdesM=XdesM, 
							dimXdes=dimXdes, gwt0=gwt0, dat=dat, I=I, resp.ind.list=resp.ind.list, G=G, 
							use.freqpatt=use.freqpatt, ind.group=ind.group, weights=weights, 
							Xlambda.constr.V=Xlambda.constr.V, e2=e2, V1=V1, Xlambda_positive=Xlambda_positive ) 
			#-- apply general acceleration function
			res <- cdm_pem_acceleration( iter=iter, pem_parameter_index=pem_parameter_index, 
						pem_parameter_sequence=pem_parameter_sequence, pem_pars=pem_pars, 
						PEM_itermax=PEM_itermax, parmlist=parmlist, ll_fct=ll_fct, ll_args=ll_args, deviance.history=deviance.history )
			#-- collect output					
			PEM <- res$PEM
			pem_parameter_sequence <- res$pem_parameter_sequence
			cdm_pem_acceleration_assign_output_parameters( res_ll_fct=res$res_ll_fct, 
							vars=pem_output_vars , envir=envir, update=res$pem_update ) 			
		}			
		 
		#*****
		#8 calculate likelihood
		# n.ik [ TP , I , K+1 , G ]
		# N.ik [ TP , I , G ]
		# probs [I , K+1 , TP ]
		ll <- slca_calc_likelihood( G=G, use.freqpatt=use.freqpatt, ind.group=ind.group, p.xi.aj=p.xi.aj, pi.k=pi.k, 
					weights=weights ) 							
		dev <- -2*ll	
		deviance.history[iter+1] <- dev					

		#---- display progress		
		Xlambda_change <- gg1 <- abs( Xlambda - Xlambda0 )
		pardiff <- max( gg1 )
		deltadiff <- abs( pi.k - pi.k0 )	
		conv1 <- max( c(pardiff,deltadiff))
		globconv1 <- abs( dev - dev0) 
		iter <- iter +1
		#---- print progress
        slca_print_progress_em_algorithm( progress=progress, disp=disp, iter=iter, dev=dev, dev0=dev0, deltadiff=deltadiff, 
				Xlambda_change=pardiff, regularization=regularization, regular_penalty=regular_penalty ) 
		if ( globconv1 < globconv ){
			iterate <- FALSE
		}
		
		# save values corresponding to minimal deviance
		if ( ( dev < mindev ) | ( iter == 1 ) ){
			Xlambda.min <- Xlambda
			se.Xlambda.min <- se.Xlambda
			pi.k.min <- pi.k
			n.ik.min <- n.ik
			probs.min <- probs
			delta.min <- delta
			covdelta.min <- covdelta
			mindev <- dev
			iter.min <- iter
		}
		
	}
	############################################
	# END MML Algorithm
	############################################
		
	Xlambda.min -> Xlambda
	se.Xlambda.min -> se.Xlambda
	pi.k.min -> pi.k
	n.ik.min -> n.ik
	probs.min -> probs
	delta.min -> delta
	covdelta.min -> covdelta
	mindev -> dev
	# iter.min -> iter
		
	# names
	if ( is.null(dimnames(Xdes)[[4]] ) ){
		dimnames(Xdes)[[4]] <- paste0("lam" , 1:Nlam ) 
	}
	if ( is.null(dimnames(Xdes)[[3]] ) ){
		dimnames(Xdes)[[3]] <- paste0("Class" , 1:TP ) 
	}	
		
	names(Xlambda) <- dimnames(Xdes)[[4]]
	colnames(pi.k) <- paste0("Group" , 1:G )
	rownames(pi.k) <- dimnames(Xdes)[[3]]		
		
	#  collect item parameters
	item1 <- array( aperm( probs , c(2,1,3)) , dim= c(I*maxK , TP) )
	colnames(item1) <- dimnames(Xdes)[[3]] 
	item <- data.frame("item" = rep(colnames(dat) , each=maxK) , 
						"Cat" = rep(0:K , I) , item1 )			
	rownames(item) <- paste0( rep(colnames(dat) , each=maxK) , "_Cat" , rep(0:K , I) )		
		
	#-- Information criteria
    ic <- slca_calc_ic( dev=dev, dat=dat, G=G, K=K, TP=TP, I=I, delta.designmatrix=delta.designmatrix, delta.fixed=delta.fixed, 
				Xlambda=Xlambda, Xlambda.fixed=Xlambda.fixed, data0=data0, deltaNULL=deltaNULL, 
				Xlambda.constr.V=Xlambda.constr.V, regularization=regularization, 
				regular_indicator_parameters=regular_indicator_parameters, Xlambda_positive=Xlambda_positive ) 
	#########################################
	# item fit [ items , theta , categories ] 
	# # n.ik [ 1:TP , 1:I , 1:(K+1) , 1:G ]
	probs <- aperm( probs , c(3,1,2) )
#	itemfit.rmsea <- itemfit.rmsea( n.ik , pi.k , probs ,
#			itemnames = colnames(data) )		
#   item$itemfit.rmsea <- itemfit.rmsea$rmsea

	# person parameters
	# ...
	
	# person parameters
	mle.class <- max.col( p.xi.aj ) 
	map.class <- max.col( p.aj.xi ) 
			
	#*************************
	# collect output	
	s2 <- Sys.time()
	time <- list( s1=s1,s2=s2 , timediff=s2-s1)
	control <- list()
	control$weights <- weights
	control$group <- group	
	res <- list( item=item, deviance=dev, ic=ic, Xlambda=Xlambda, se.Xlambda=se.Xlambda, pi.k=pi.k, pjk=probs, 
				n.ik=n.ik, G=G, I=I, N=n, TP=TP, delta=delta, covdelta=covdelta, 
				delta.designmatrix=delta.designmatrix, MLE.class=mle.class, MAP.class=map.class, 
				data=data, group.stat=group.stat, p.xi.aj=p.xi.aj, posterior=p.aj.xi, K.item=K.item, 
				time=time, iter=iter, iter.min=iter.min, converged=iter<maxiter, 
				deviance.history=deviance.history, AIC=ic$AIC, BIC=ic$BIC, Npars=ic$np, loglike=-dev/2, 
				seed.used=seed.used, PEM=PEM, Xlambda.init=Xlambda.init, delta.init=delta.init, 
				Xlambda_positive=Xlambda_positive, regular_penalty=regular_penalty, regular_n=regular_n, 
				regular_lam=regular_lam, regular_w=regular_w, regular_lam_used=regular_lam_used,
				regular_indicator_parameters=regular_indicator_parameters, regularization=regularization, 
				control=control, call=cl ) 		
	class(res) <- "slca"
	#--- print progress
	slca_print_progress_end( s1=s1, s2=s2, progress=progress ) 
	return(res)				
}		
###################################################
