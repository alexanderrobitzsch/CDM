## File Name: gdina.R
## File Version: 9.127


################################################################################
# GDINA Model
################################################################################

gdina <- function( data, q.matrix, skillclasses=NULL , conv.crit = 0.0001, 
					dev.crit = .1 , maxit = 1000,
					linkfct = "identity" , Mj = NULL , 
					group = NULL , 
					invariance = TRUE , 
					method = NULL , 
					delta.init = NULL , 
					delta.fixed = NULL ,
					delta.designmatrix = NULL , 
					delta.basispar.lower = NULL , 
					delta.basispar.upper = NULL , 					
					delta.basispar.init = NULL , 
					zeroprob.skillclasses = NULL , 
					attr.prob.init = NULL , 
					reduced.skillspace=TRUE , 
					reduced.skillspace.method=2 , 
					HOGDINA = -1 , 
					Z.skillspace = NULL , 
                    weights = rep( 1, nrow( data ) ),  rule = "GDINA", 
                    progress = TRUE , 
					progress.item = FALSE , 
					mstep_iter = 10 ,
					mstep_conv = 1E-4 , 
					increment.factor = 1.01 ,
					fac.oldxsi = 0 ,
					max.increment = .3 ,
					avoid.zeroprobs = FALSE , 
					seed = 0 , 		
					save.devmin=TRUE , calc.se = TRUE ,
					se_version = 1 , PEM = FALSE , PEM_itermax = maxit , 
					...
						){
                    
# data: a required matrix of binary response data, whereas the items are in the columns 
#       and the response pattern in the rows. NA values are allowed.
#
# q.matrix: a required binary matrix describing which attributes are required, coded by 1,
#       and which attributes are not required, coded by 0, to master the items, whereas the
#       attributes are in the columns and the items in the rows.
#
# method: WLS (using W matrix) or ULS (without a W matrix) estimation
#
# conv.crit: termination criterion of the iterations defined as the maximum change in parameter
#       estimates. Iteration ends if maximal parameter change is below this value.
#
# maxit: maximal number of iterations.
#
# zeroprob.skillclasses:  an optional vector of integers which indicates which skill classes should have
#							zero probability
#
# weights: an optional vector of weights for the response pattern. Noninteger weights allow for different
#       sampling schemes.
#
# weight.matrix: use weighting matrix in least squares estimation

# rule: an optional character string or vector of character strings specifying the model rule that is used. 
#       The character strings must be of "DINA" or "DINO". If a vector of character strings is specified, 
#       implying an itemwise condensation rule, the vector must be of length ncol(data). The default is the 
#       condensation rule "DINA" for all items.
#		See help: DINA, DINO, ACDM (=GDINA1), GDINA1, GDINA2
#		The saturated specification GDINA is the default.
#
# progress: an optional logical indicating whether the function should print the progress of iteration.

	CALL <- match.call()

	if (progress){
		cat("---------------------------------------------------------------------------------\n")
		d1 <- packageDescription("CDM")
		cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , "\n" )		
	}
    time1 <- list( "s1" = Sys.time() )
	cl <- match.call()

	#########################################################
	# treat sequential items
	#########################################################

	res <- gdina_proc_sequential_items( data=data , q.matrix = q.matrix )
	data <- res$data
	sequential <- res$sequential
	q.matrix <- res$q.matrix

	#########################################################
	# in case of item parameter noninvariance restructure dataset
	#########################################################

	res <- gdina_proc_noninvariance_multiple_groups( data=data, q.matrix=q.matrix, 
				invariance=invariance, group=group )
	data <- res$data
	q.matrix <- res$q.matrix
	
	########################################################
	# add item and attribute labels	if necessary
	########################################################

	if ( is.null( colnames( data ) ) ){
		colnames(data) <- paste( "Item" , seq(1,ncol(data)) , sep="")
	}
	if ( is.null( colnames( q.matrix ) ) ){
		colnames(q.matrix) <- paste( "Attr" , seq(1,ncol(q.matrix)) , sep="")
	}

	################################################################################
	# check consistency of input (data, q.matrix, ...)                             #
	################################################################################

	dat.items <- data
	
	#---- check of admissible rules
	res <- gdina_proc_check_admissible_rules(rule=rule)
	
	#---- RRUM model specifications
	res <- gdina_proc_spec_rrum( rule=rule, method=method, linkfct=linkfct ) 
	rrum.params <- res$rrum.params
	rrum.model <- res$rrum.model
	method <- res$method
	linkfct <- res$linkfct	
	rule <- res$rule
					
	################################################################################
	# model specification: DINA, DINO or itemwise specification of DINA or DINO    #
	################################################################################

	r1 <- "GDINA Model"

	################################################################################
	# multiple group estimation
	################################################################################
	
	res <- gdina_proc_multiple_group_objects(group=group)
	G <- res$G
	group <- res$group
	group0 <- res$group0
	groupre <- res$groupre
	group.stat <- res$group.stat
	group2 <- res$group2
																	
	#---- parameters for HOGDINA model
	if (HOGDINA >= 0){	
		res <- gdina_proc_hogdina_theta_distribution(G=G)	
		theta.k <- res$theta.k
		reduced.skillspace <- res$reduced.skillspace
		wgt.theta <- res$wgt.theta			
	}
															
	################################################################################
	# display on R console                                                         #
	################################################################################

    disp <- r1      
	s1 <- Sys.time()
	#--- display progress
	res <- gdina_progress_start_estimation( progress=progress, linkfct=linkfct, disp=disp, 
				G=G, groupre=groupre, s1=s1 ) 
			
	################################################################################
	# definition of model parameters                                               # 
	################################################################################

	res <- gdina_proc_define_model_parameters( dat.items=dat.items, q.matrix=q.matrix, 
					rule=rule, HOGDINA=HOGDINA, G=G ) 
	rule <- res$rule
	dat.items <- res$dat.items
	q.matrix <- res$q.matrix
	a.attr <- res$a.attr
	b.attr <- res$b.attr
	I <- res$I
	J <- res$J
	K <- res$K
						
	a0 <- Sys.time()
						
	################################################################################
	# Initialization and missing data handling                                     #
	################################################################################
    
    # recode missing data by 9
    resp <- 1 - is.na(dat.items)
    dat.items[ resp == 0 ] <- 9
    
    #--- standardize weights such that the sum of defined weights is equal to the number of rows in the data frame
    weights <- gdina_standardize_weights( weights=weights )
		
	################################################################################
	# calculate item response patterns                                             #
	################################################################################

	res <- gdina_proc_item_response_patterns( dat.items=dat.items, J=J, G=G, weights=weights, group=group ) 
	item.patt.subj <- res$item.patt.subj
	item.patt <- res$item.patt
	six <- res$six
	item.patt.freq <- res$item.patt.freq
		 		 
	################################################################################ 
	# generate all attribute patterns                                              #
	################################################################################

	res <- gdina_create_attribute_patterns( q.matrix=q.matrix, skillclasses=skillclasses, 
				zeroprob.skillclasses=zeroprob.skillclasses, Z.skillspace=Z.skillspace, 
				G=G, reduced.skillspace=reduced.skillspace ) 
	K <- res$K
	maxAttr <- res$maxAttr
	attr.patt <- res$attr.patt
	L <- res$L
	attr.patt.c <- res$attr.patt.c
	reduced.skillspace <- res$reduced.skillspace
	Z.skillspace <- res$Z.skillspace
	Z <- res$Z
	beta <- res$beta
	covbeta <- res$covbeta
	ncolZ <- res$ncolZ		
	q.entries <- res$q.entries
	
	if ( reduced.skillspace ){
		res <- gdina_attribute_patterns_reduced_skillspace( attr.patt=attr.patt, K=K, maxAttr=maxAttr, q.matrix=q.matrix, 
					Z.skillspace=Z.skillspace ) 
		Z <- res$Z
		ncolZ <- res$ncolZ
		beta <- rep(0, ncol(Z) )
	}
			
	################################################################################
	# assign uniform prior distribution of all latent class patterns                
	################################################################################

	res <- gdina_init_class_probabilities( G=G, L=L, seed=seed, attr.prob.init=attr.prob.init ) 
	attr.prob <- res$attr.prob
					
	################################################################################
	# create design matrices 
	################################################################################	

	res <- gdina_create_designmatrices( J=J, Mj=Mj, Aj=Aj, q.matrix=q.matrix, rule=rule, L=L, attr.patt=attr.patt ) 
	Mj <- res$Mj
	Mj.userdefined <- res$Mj.userdefined
	Aj <- res$Aj
	Nattr.items <- res$Nattr.items
	necc.attr <- res$necc.attr
	aggr.attr.patt <- res$aggr.attr.patt
	attr.items <- res$attr.items
	aggr.patt.designmatrix <- res$aggr.patt.designmatrix
	Mj.index <- res$Mj.index	
	
	###############################################################################
	# initial item parameters
	###############################################################################
	
	delta <- gdina_init_item_parameters( delta.init=delta.init, linkfct=linkfct, J=J, seed=seed, Mj=Mj, 
				delta.basispar.init=delta.basispar.init, delta.designmatrix=delta.designmatrix, Mj.index=Mj.index,
				rule=rule ) 
		
	#------ some compute inverse matrices for least squares estimation
	invM.list <- gdina_proc_uls_inverse_matrices(Mj=Mj, J=J)

 	if ( fac.oldxsi>= 1){ 
		fac.oldxsi <- 0 
	}
	djj_old <- as.list( 1:J )
				
	################################################################################
	# some prelimaries for EM algorithm                                            #  
	################################################################################

	res <- gdina_proc_split_item_response_patterns( item.patt=item.patt, J=J )         
	IP <- res$IP
	resp.patt <- res$resp.patt
	item.patt.split <- res$item.patt.split	
   
    iter <- 1 # Iteration number
    likediff <- 1 # Difference in likelihood estimates
    loglike <- 0 # init for log-Likelihood
    
    # init value for maximum parameter change in likelihood maximization
    max.par.change <- 1000
    devchange <- 1000
	
	# analyze response patterns if there are some missings
	cmresp <- colMeans( resp.patt )
    some.missings <- mean(cmresp) < 1
	
    # calculations for expected counts
	# response indicator list
    resp.ind.list <- list( 1:J )
	for (i in 1:J){ 
		resp.ind.list[[i]] <- which( resp.patt[,i] == 1)  
	}
	
	# this matrix ipr is needed for computing R.lj
	if (G==1){
		ipr <- item.patt.split * item.patt.freq*resp.patt
	}
       
    disp <- "...........................................................\n"		

	#** for reduced skillspace
	if (reduced.skillspace){
		item_patt_freq_matr <- outer( item.patt.freq , rep( 1 , L) )
		
	}
    
	#--- delta parameter indices
	res <- gdina_proc_delta_indices(delta=delta, Mj=Mj)
	delta_indices <- res$delta_indices
	delta_partable <- res$delta_partable
	delta_vec <- unlist(delta)

	# reconvert vector into a list
    delta <- gdina_delta_convert_into_list( delta_vec=delta_vec, delta_indices=delta_indices, J=J ) 
				
	#-- preliminaries PEM acceleration
	if (PEM){	
		envir <- environment()	
		if (! reduced.skillspace){
			pem_pars <- c("delta_vec","attr.prob")
		}
		if ( reduced.skillspace){
			pem_pars <- c("delta_vec","beta")
		}
		pem_output_vars <- unique( c( pem_pars , "delta.new","attr.prob") )
		parmlist <- cdm_pem_inits_assign_parmlist(pem_pars=pem_pars, envir=envir)
		res <- cdm_pem_inits( parmlist=parmlist)
		pem_parameter_index <- res$pem_parameter_index
		pem_parameter_sequence <- res$pem_parameter_sequence				
	}
	
	deviance.history <- rep(NA, maxit)
	
	#********************************
	# extract parameters with minimal deviances

    dev.min <- 1E99
	R.lj.gg <- I.lj.gg <- NULL
	suffstat_probs <- as.list(1:J)
		
	devchange <- 0	
	#*********************************

	################################################################################
	# BEGIN OF THE ITERATION LOOP                                                  #
	################################################################################
    
    while ( ( iter <= maxit ) & 
				( ( max.par.change > conv.crit ) | ( devchange > dev.crit  ) )	)
	{

		################################################################################
		# STEP I:                                                                      #
		# calculate P(X_i | alpha_l):                                                  # 
		# probability of each item response pattern given an attribute pattern         #
		################################################################################
	
		#--- calculate item response probabilities
		pjM <- gdina_calc_prob( progress=progress, iter=iter, disp=disp, J=J, L=L, 
					aggr.attr.patt=aggr.attr.patt, Mj=Mj, delta=delta, linkfct=linkfct ) 
		#--- calculate individual likelihood
		p.xi.aj <- gdina_calc_individual_likelihood( IP=IP, L=L, pjM=pjM, item.patt.split=item.patt.split, 
						J=J, resp.ind.list=resp.ind.list, zeroprob.skillclasses=zeroprob.skillclasses ) 
							
		################################################################################
		# STEP II:                                                                     #
		# calculate P(  \alpha_l | X_i ):                                              #
		# posterior probability of each attribute pattern given the item response pattern
		################################################################################

		res <- gdina_calc_individual_posterior( G=G, IP=IP, attr.prob=attr.prob, p.xi.aj=p.xi.aj, 
					L=L, I=I, zeroprob.skillclasses=zeroprob.skillclasses, 
					reduced.skillspace=reduced.skillspace, item.patt.freq=item.patt.freq )                                           
		p.aj.xi <- res$p.aj.xi
		attr.prob <- res$attr.prob		
		
		#######################################################################
		# STEP II0: higher order GDINA model
		#######################################################################
		
		if (HOGDINA >= 0){
			res <- gdina_attribute_structure_hogdina( G=G, attr.prob=attr.prob, attr.patt=attr.patt, 
						wgt.theta=wgt.theta, HOGDINA=HOGDINA, a.attr=a.attr, b.attr=b.attr, theta.k=theta.k ) 
			a.attr <- res$a.attr
			b.attr <- res$b.attr
			attr.prob <- res$attr.prob
		}
		
		#######################################################################
		# STEP IIa: reduction of skill space					
		#######################################################################

		# This currently only works in case of a single group
		if (reduced.skillspace){			
			ntheta <- colSums( item_patt_freq_matr*p.aj.xi )
			res <- gdina_reduced_skillspace( ntheta=ntheta, Z=Z, reduced.skillspace.method=reduced.skillspace.method )
			beta <- res$beta
			attr.prob <- res$attr.prob
			pred.ntheta <- res$pred.ntheta
		}
		 
		################################################################################
		# STEP III:                                                                    #
		# calculate I_{lj} and R_{lj}                                                  #
		# for a derivation see De La Torre (2008, Journal of Educational and           #
		# Behavioral Statistics)                                                       #
		# I_{lj} ... expected frequency of persons in attribute class l for item j     #
		#               (in case of no missing data I_{lj} = I_l for all items j       #
		# R_{lj} ... expected frequency of persons in attribute class l for item j     #
		#               which correctly solve item j                                   #
		################################################################################

		res <- gdina_calc_expected_counts( G=G, J=J, L=L, item.patt.freq=item.patt.freq, p.aj.xi=p.aj.xi, 
					some.missings=some.missings, ipr=ipr, attr.patt.c=attr.patt.c, resp.patt=resp.patt,
					item.patt.split=item.patt.split, data=data) 
		I.lj <- res$I.lj
		R.lj <- res$R.lj
		I.lj.gg <- res$I.lj.gg
		R.lj.gg <- res$R.lj.gg
			
		################################################################################
		# STEP IV:                                                                     #
		# M Step																	   # 
		# GDINA Model																   #
		################################################################################

		res <- gdina_mstep_item_parameters( R.lj=R.lj, I.lj=I.lj, aggr.patt.designmatrix=aggr.patt.designmatrix, 
					max.increment=max.increment, increment.factor=increment.factor, J=J, Aj=Aj, Mj=Mj, delta=delta, 
					method=method, avoid.zeroprobs=avoid.zeroprobs, invM.list=invM.list, linkfct=linkfct, rule=rule, 
					iter=iter, fac.oldxsi=fac.oldxsi, rrum.model=rrum.model, delta.fixed=delta.fixed, devchange=devchange, 
					mstep_iter=mstep_iter, mstep_conv=mstep_conv, Mj.index=Mj.index, suffstat_probs=suffstat_probs ) 	
		delta.new <- res$delta.new
		suffstat_probs <- res$suffstat_probs
	
		##########################################################################
		# estimation with a design matrix for delta parameters
		##########################################################################
		if ( ! is.null( delta.designmatrix ) ){ 
			delta.new <- gdina_mstep_item_parameters_designmatrix( delta.new=delta.new, delta.designmatrix=delta.designmatrix, 
								delta.basispar.lower=delta.basispar.lower, 
								delta.basispar.upper=delta.basispar.upper, Mj.index=Mj.index , J=J) 		
		}

		delta_vec <- unlist(delta.new)
			
		#-- PEM acceleration
		if (PEM){
			#-- collect all parameters in a list
			parmlist <- cdm_pem_inits_assign_parmlist(pem_pars=pem_pars, envir=envir)			
			#-- define log-likelihood function
			ll_fct <- gdina_calc_loglikelihood
			#- extract parameters
			ll_args <- list( delta_vec=delta_vec, beta=beta, attr.prob=attr.prob, Z=Z, delta_indices=delta_indices, J=J, 
					iter=iter, disp=disp, L=L, aggr.attr.patt=aggr.attr.patt, Mj=Mj, linkfct=linkfct, IP=IP, 
					item.patt.split=item.patt.split, resp.ind.list=resp.ind.list, 
					zeroprob.skillclasses=zeroprob.skillclasses, item.patt.freq=item.patt.freq, 
					loglike=loglike, G=G, reduced.skillspace=reduced.skillspace ) 
			#-- apply general acceleration function (take care of the correct iteration index:
			#    it must start at zero)
			res <- cdm_pem_acceleration( iter=iter-1, pem_parameter_index=pem_parameter_index, 
						pem_parameter_sequence=pem_parameter_sequence, pem_pars=pem_pars, 
						PEM_itermax=PEM_itermax, parmlist=parmlist, ll_fct=ll_fct, ll_args=ll_args,
						deviance.history=deviance.history )
			#-- collect output					
			PEM <- res$PEM
			pem_parameter_sequence <- res$pem_parameter_sequence
			cdm_pem_acceleration_assign_output_parameters( res_ll_fct=res$res_ll_fct, 
							vars=pem_output_vars , envir=envir, update=res$pem_update ) 			
		}			
		
		
		#################################################

		#--- calculate deviance
		res <- gdina_calc_deviance( p.xi.aj=p.xi.aj, attr.prob=attr.prob, item.patt.freq=item.patt.freq, 
					loglike=loglike, G=G, IP=IP ) 
		like.new <- res$like.new
		likediff <- res$likediff
		loglikeold <- loglike
		loglike <- like.new
		
		#--- maximum parameter change
		max.par.change <- gdina_maximum_parameter_change( delta=delta, delta.new=delta.new, linkfct=linkfct ) 
		delta <- delta.new	  # reset delta parameter estimates
	
		#--- progress EM algorithm
		res <- gdina_progress_em_algorithm( delta=delta, data=data, like.new=like.new, loglikeold=loglikeold, 
					max.par.change=max.par.change, iter=iter, progress=progress, progress.item=progress.item ) 
		utils::flush.console() # Output is flushing on the console
		iter <- iter + 1 # new iteration number                                    
		devchange <- abs( 2*(like.new-loglikeold) )
   	
		#**** update parameters at minimal deviance
		dev <- -2*like.new	
		deviance.history[iter-1] <- dev		
		if (save.devmin){		
			if ( dev < dev.min ){
				iter.min <- iter-1	
				delta.min <- delta
				dev.min <- dev
				p.aj.xi.min <- p.aj.xi
				p.xi.aj.min <- p.xi.aj
				R.lj.min <- R.lj
				I.lj.min <- I.lj		
				attr.prob.min <- attr.prob
				loglike.min <- loglike
			}		
		}
		#********************************
		
	}	
	################################################################################
	# END OF THE ITERATION LOOP                                                    #
	################################################################################

	#***************************************
	# use parameters with minimal deviance
	iterused <- iter - 1
	if (save.devmin){
		iter.min -> iter	
		delta.min -> delta
		dev.min -> dev
		p.aj.xi.min -> p.aj.xi
		p.xi.aj.min -> p.xi.aj
		R.lj.min -> R.lj
		I.lj.min -> I.lj		
		attr.prob.min -> attr.prob
		loglike.min -> loglike		
		}
	#****************************************

	#--- pattern output
	res <- gdina_post_pattern_output( G=G, p.xi.aj=p.xi.aj, zeroprob.skillclasses=zeroprob.skillclasses, 
				item.patt=item.patt, attr.patt.c=attr.patt.c, p.aj.xi=p.aj.xi, item.patt.subj=item.patt.subj, 
				group2=group2, attr.patt=attr.patt, K=K ) 
	pattern <- res$pattern
	p.xi.aj <- res$p.xi.aj
	
	#####################################################
	# itemwise standard error calculation
	res <- gdina_post_calc_se( G=G, p.aj.xi=p.aj.xi, item.patt.freq=item.patt.freq, attr.prob=attr.prob, 
				p.xi.aj=p.xi.aj, IP=IP, J=J, calc.se=calc.se, aggr.attr.patt=aggr.attr.patt, Aj=Aj, Mj=Mj, R.lj=R.lj, 
				I.lj=I.lj, item.patt.split=item.patt.split, resp.patt=resp.patt, delta=delta, linkfct=linkfct, rule=rule, 
				avoid.zeroprobs=avoid.zeroprobs, data=data, se_version=se_version, method=method, delta.fixed=delta.fixed, 
				q.matrix=q.matrix )
	varmat.delta <- res$varmat.delta
	varmat.palj <- res$varmat.palj
	se.delta <- res$se.delta
	delta.summary <- res$delta.summary
	freq.pattern <- res$freq.pattern
	item.patt.freq <- res$item.patt.freq
		
	# compute RRUM parametrization if model is specified
	if (rrum.model){
		rrum.params <- .rrum.param( delta.summary=delta.summary, q.matrix=q.matrix )
	}
	
	#--- skill pattern and attribute pattern
	res <- gdina_post_skill_pattern( attr.prob=attr.prob, G=G, attr.patt.c=attr.patt.c, K=K, maxAttr=maxAttr, 
				q.matrix=q.matrix, q.entries=q.entries, attr.patt=attr.patt ) 
	attr.prob <- res$attr.prob
	skill.patt <- res$skill.patt
	
	#------- calculation of the AIC und BIC        
	res <- gdina_calc_ic( delta=delta, delta.designmatrix=delta.designmatrix, delta.fixed=delta.fixed, 
				G=G, ncolZ=ncolZ, K=K, HOGDINA=HOGDINA, item.patt.freq=item.patt.freq, 
				zeroprob.skillclasses=zeroprob.skillclasses, loglike=loglike )
	Npars <- res$Npars
	aic <- res$aic
	bic <- res$bic
	caic <- res$caic
	Nskillpar <- res$Nskillpar
	Nipar <- res$Nipar	
	
	#--- postprocess posterior distributions	
	res <- gdina_post_posterior_output( G=G, p.aj.xi=p.aj.xi, p.xi.aj=p.xi.aj, pattern=pattern, data=data, 
				item.patt.subj=item.patt.subj, item.patt=item.patt, attr.prob=attr.prob, group=group ) 
	item.patt.subj <- res$item.patt.subj
	attr.prob <- res$attr.prob
	p.xi.aj <- res$p.xi.aj
	posterior <- res$posterior
	pattern <- res$pattern
	attr.prob0 <- res$attr.prob0	
				
	#--- item fit [ items , theta , categories ] 
	res <- gdina_itemfit( L=L, J=J, R.lj=R.lj, I.lj=I.lj, item.patt.freq=item.patt.freq, G=G, 
				attr.prob=attr.prob, data=data, pjM=pjM ) 
	itemfit.rmsea <- res$itemfit.rmsea
	pi.k <- res$pi.k
	n.ik <- res$n.ik
	pi.k <- res$pi.k	
	
	#---- calculate model implied probabilities	
	probitem <- gdina_probitem( Mj=Mj, Aj=Aj, delta=delta, rule=rule, linkfct=linkfct, 
					delta.summary=delta.summary ) 	
					
	#***************************** OUTPUT **********************************
	if (progress){
		cat("---------------------------------------------------------------------------------\n")
	}
	iter <- iterused
    res <- list( coef=delta.summary, item=delta.summary, delta=delta, se.delta=se.delta, 
			    probitem=probitem, itemfit.rmsea=itemfit.rmsea, mean.rmsea=mean(itemfit.rmsea),	
				loglike=loglike, deviance=-2*loglike, G=G, N=colSums( as.matrix(item.patt.freq) ), 
				AIC=aic, BIC=bic, CAIC=caic, Npars =Npars, Nipar=Nipar , Nskillpar=Nskillpar,
				Nskillclasses=L, varmat.delta=varmat.delta, varmat.palj=varmat.palj,
                posterior=posterior, like=p.xi.aj, data=data, q.matrix=q.matrix,
                pattern=pattern, attribute.patt=attr.prob, skill.patt=skill.patt,
                subj.pattern=item.patt.subj, attribute.patt.splitted=attr.patt, 
				pjk=pjM,  Mj=Mj, Aj=Aj, rule=rule, linkfct=linkfct, delta.designmatrix=delta.designmatrix, 
				reduced.skillspace=reduced.skillspace, Z.skillspace=if(reduced.skillspace){ Z } else { NULL }, 
				beta=beta, covbeta=covbeta, display=disp, item.patt.split=item.patt.split, 
				item.patt.freq=item.patt.freq, model.type=r1, iter=iter, iterused=iterused, rrum.model=rrum.model,
				rrum.params= rrum.params, group.stat=group.stat,  NAttr=maxAttr, invariance=invariance, 
				HOGDINA=HOGDINA, seed= seed, iter=iter, converged=iter < maxit ,
				deviance.history=deviance.history)
		 
	if (HOGDINA>=0) { 
	    colnames(a.attr) <- paste0( "a.Gr" , 1:G )
		colnames(b.attr) <- paste0( "b.Gr" , 1:G )
		rownames(b.attr) <- rownames(a.attr) <- colnames(q.matrix)
		res$a.attr <- a.attr 
		res$b.attr <- b.attr
		res$attr.rf <- cbind( b.attr , a.attr )
	}						
	# computation time
    time1$s2 <- Sys.time()
	res$time <- time1
	# res$time$timediff <- print(res$time$s2 - res$time$s1)	
	res$time$timediff <- res$time$s2 - res$time$s1	
	if ( progress ){
		print(res$time$s2 - res$time$s1)	
	}
	
	# control parameter
	control <- list( skillclasses=skillclasses, q.matrix=q.matrix, conv.crit=conv.crit, 
					dev.crit=dev.crit, maxit=maxit, linkfct=linkfct, Mj=Mj, Aj=Aj, 
					group=group, method=method, delta.designmatrix=delta.designmatrix, 
					delta.basispar.lower=delta.basispar.lower, delta.basispar.upper=delta.basispar.upper, 					
					delta.basispar.init=delta.basispar.init, zeroprob.skillclasses=zeroprob.skillclasses, 
					reduced.skillspace=reduced.skillspace, HOGDINA=HOGDINA, Z.skillspace=Z.skillspace, 
                    weights=weights, rule=rule, I.lj=I.lj, R.lj=R.lj, I.lj.gg=I.lj.gg, 
					R.lj.gg=R.lj.gg, aggr.patt.designmatrix=aggr.patt.designmatrix,	Mj.index=Mj.index, method=method,
					aggr.attr.patt=aggr.attr.patt,IP=IP, p.aj.xi=p.aj.xi,item.patt.split=item.patt.split,
					resp.patt=resp.patt,freq.pattern=freq.pattern, item.patt.freq=item.patt.freq,invM.list=invM.list,
					item.patt.subj=item.patt.subj, item.patt=item.patt, suffstat_probs=suffstat_probs, 					
					increment.factor=increment.factor, fac.oldxsi=fac.oldxsi, avoid.zeroprobs=avoid.zeroprobs,
					attr.prob=attr.prob0, delta.fixed=delta.fixed, sequential=sequential,
					invariance=invariance, se_version=se_version ) 	
	res$control <- control			
    #--- create parameter table	
	res$partable <- gdina_partable(res)	
	#--- polychoric correlations
	res$polychor <- CDM.calc.polychor(res)	
	res$call <- cl
    class(res) <- "gdina"
    return(res)
}
##################################################################



