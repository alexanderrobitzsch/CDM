## File Name: gdm.R
## File Version: 8.655


###########################################
# General Diagnostic Model
###########################################
#
#..........................#
# OUTLINE:                   #
#..........................#
#
# Input:
# ------
# data ... polytomous responses
#     I Items with categories 0,1,...,K
# group ... 1, ..., G
#     covariates and groups. Up to now, only use groups
# theta.k ... Multidimensional ability design vector
#            as input. It is of dimension D and therefore a
#             a [TH,D] matrix (TH is the number of theta points)
#     Design matrix for smoothing of theta.k distribution
#
# Model:
# ------
# P(X_{pi}=k)=g( b_ik + a_i1 q_i1k theta_i1 + ... + a_iD q_iD*k theta_iD )
# b_ik ... item-category difficulty
# a_id ... item slopes
# q_ik ... design matrix for slopes
#    * in general it will be (0,1,...,K)
#     but can be specified by the user
#   * The theta vectors can also be transformed in the R formula
#     framework. Therefore D* will in general be larger
#     than D. For example, an interaction \theta_1*\theta_2 or
#     theta^2 can be used in the estimation.
#
# Algorithm:
# ----------
# b ... b[1:I,1:K]  matrix of difficulties
# a ... a[1:I,1:D] array of item slopes
# n_tikg n.ik[1:TH,1:I,1:K,1:G] array of expected item responses
#        of ability pattern t at item i in category k and group g
# pi.k ... [1:TH,1:G] marginal ability distribution in groups 1,...,G
#
# Specifications:
# ---------------
# o Q matrix: allocate items to dimensions:
#      can be either a matrix or an array
# o matrix with fixed b or a parameters
# o itemgroups for a and b parameters / constraints
#    est.a and est.b
# o log-linear smoothing of ability distribution
#     -> design matrix of theta
#
# Details:
# --------
# o If there are different categories per item,
#    then use the maximal category K for all items and
#   set b_{ik} to infinity if for item i categeory k
#   is not observed.
# o Calculations of probabilities:
#    P(X_pi=k|\theta)=exp( b_{ik} + a_i1 * q_{i1k} theta_1 +
#                            ... + a_{iD} * q_{i1D} theta_D ) / NN
#    The normalization constraint NN must be calculated
#   appropriately .
#
#################################################################
# GDM function
#
gdm <- function( data, theta.k, irtmodel="2PL", group=NULL,
            weights=rep(1, nrow(data)),
            Qmatrix=NULL,thetaDes=NULL, skillspace="loglinear",
            b.constraint=NULL, a.constraint=NULL,
            mean.constraint=NULL, Sigma.constraint=NULL,
            delta.designmatrix=NULL, standardized.latent=FALSE,
            centered.latent=FALSE, centerintercepts=FALSE, centerslopes=FALSE,
            maxiter=1000, conv=1E-5, globconv=1E-5, msteps=4,
            convM=.0005, decrease.increments=FALSE, use.freqpatt=FALSE,
            progress=TRUE, PEM=FALSE, PEM_itermax=maxiter, ...)
{
    # mean.constraint [ dimension, group, value ]
    # Sigma.constraint [ dimension1, dimension2, group, value ]
    #*************************
    # data preparation
    s1 <- Sys.time()
    e1 <- environment()
    cl <- match.call()
    ## prevent from warnings in R CMD check "no visible binding"
    ## gdm: no visible binding for global variable 'TD'
    TD <- TP <- EAP.rel <- mean.trait <- sd.trait <- skewness.trait <- NULL
    K.item <- correlation.trait <- D <- NULL
    se.theta.k <- NULL
    data0 <- data <- as.matrix(data)
    dat.resp0 <- dat.resp <- 1 - is.na(data)
    dat <- data
    dat[ is.na(data) ] <- 0
    dat0 <- dat
    # center slopes
    if ( irtmodel!="2PL" ){
        centerslopes <- FALSE
    }
    # use frequency pattern. If yes, then some data preparation follows.
    if ( use.freqpatt ){
        res <- gdm_data_prep( dat=dat, data=data, weights=weights, group=group )
        weights <- res$weights
        dat <- res$dat
        dat.resp <- res$dat.resp
        data <- res$data
        item.patt <- res$item.patt
    }
    # maximal categories
    K <- max(dat)
    # list of indicator data frames
    dat.ind <- as.list( 1:(K+1) )
    for (ii in 0:K){
        dat.ind[[ii+1]] <- 1 * ( dat==ii )*dat.resp
    }
    I <- ncol(dat)    # number of items
    n <- nrow(dat)
    #--- response patterns
    resp.ind.list <- gdm_proc_response_indicators(dat.resp=dat.resp)

    #-- process data for multiple groups
    res <- slca_proc_multiple_groups( group=group, n=n )
    G <- res$G
    group <- res$group
    group0 <- res$group0
    group.stat <- res$group.stat
    Ngroup <- res$Ngroup

    #--- theta design
    res <- gdm_thetadesign( theta.k=theta.k, thetaDes=thetaDes, Qmatrix=Qmatrix )
    D <- res$D
    TD <- res$TD
    TP <- res$TP
    theta.k <- res$theta.k
    thetaDes <- res$thetaDes
    Qmatrix <- res$Qmatrix

    #--- starting values for b
    b <- gdm_inits_b( dat0=dat0, dat.resp0=dat.resp0, I=I, K=K )

    #****
    # item slope matrix
    # a[1:I,1:TD] ... Items x theta dimension
    # a <- matrix( 1, nrow=I, ncol=TD )
    # item x category slopes are in principle also possible
    KK <- K    # if KK==1 then a slope parameter for all items is estimated
    a <- array( 1, dim=c(I,TD,KK) )
    # define Q matrix
    res <- gdm_Qmatrix( Qmatrix=Qmatrix, irtmodel=irtmodel, I=I, TD=TD, K=K, a=a )
    Qmatrix <- res$Qmatrix
    a <- res$a

    #--- constraints on item parameters
    res <- gdm_constraints_itempars( b.constraint=b.constraint, a.constraint=a.constraint, K=K, TD=TD,
                Qmatrix=Qmatrix, a=a )
    a.constraint <- res$a.constraint
    b.constraint <- res$b.constraint
    a <- res$a

    #--- starting values for distributions
    Sigma <- diag(1,D)
    pik <- mvtnorm::dmvnorm( matrix( theta.k,ncol=D), mean=rep(0,D), sigma=Sigma )
    pi.k <- matrix( 0, nrow=TP, ncol=G )
    for (gg in 1:G){
        pi.k[,gg] <- cdm_sumnorm( pik )
    }
    n.ik <- array( 0, dim=c(TP,I,K+1,G) )

    #***
    # extract number of skills per dimensions
    skill.levels <- rep(0,D)
    for (dd in 1:D){
        skill.levels[dd] <- length( unique(theta.k[,dd] ) )
    }

    #****
    # create thetaDes design matrix for loglinear smoothing
    res <- gdm_create_delta_designmatrix( delta.designmatrix=delta.designmatrix, TP=TP, D=D, theta.k=theta.k,
                skill.levels=skill.levels, G=G )
    delta <- res$delta
    covdelta <- res$covdelta
    delta.designmatrix <- res$delta.designmatrix

    se.a <- 0*a
    max.increment.a <- .3
    max.increment.b <- 3

    if ( standardized.latent ){
        mean.constraint <- rbind( mean.constraint, cbind( 1:D, 1, 0 )  )
        Sigma.constraint <- rbind( Sigma.constraint, cbind( 1:D, 1:D, 1, 1 ) )
        skillspace <- "normal"
    }
    if ( centered.latent ){
        mean.constraint <- rbind( mean.constraint, cbind( 1:D, 1, 0 )  )
        skillspace <- "normal"
    }

    #***
    # set constraints for a and b parameters if the maximal
    # item category differs from item to item
    res <- gdm_constraints_itempars2( b.constraint=b.constraint, a.constraint=a.constraint, K=K, TD=TD, I=I, dat=dat )
    K.item <- res$K.item
    a.constraint <- res$a.constraint
    b.constraint <- res$b.constraint

    #***
    # preparations for calc.counts
    res <- gdm_prep_calc_counts( K=K, G=G, group=group, weights=weights, dat.resp=dat.resp, dat.ind=dat.ind,
                use.freqpatt=use.freqpatt )
    ind.group <- res$ind.group
    dat.ind2 <- res$dat.ind2

    #-- preliminaries PEM acceleration
    if (PEM){
        envir <- environment()
        pem_pars <- c("b","a","pi.k")
        if ( skillspace=="est"){
            pem_pars <- c(pem_pars, "theta.k")
        }
        if ( skillspace=="loglinear"){
            pem_pars <- c(pem_pars, "delta")
        }
        pem_output_vars <- unique( c( pem_pars ) )
        parmlist <- cdm_pem_inits_assign_parmlist(pem_pars=pem_pars, envir=envir)
        res <- cdm_pem_inits( parmlist=parmlist)
        pem_parameter_index <- res$pem_parameter_index
        pem_parameter_sequence <- res$pem_parameter_sequence
    }

    deviance.history <- rep(NA, maxiter )
    gwt0 <- matrix( 1, nrow=n, ncol=TP )

    #---
    # initial values algorithm
    dev <- 0
    iter <- 0
    globconv1 <- conv1 <- 1000
    disp <- paste( paste( rep(".", 70 ), collapse=""),"\n", sep="")
    # timecat <- TRUE
    # timecat <- FALSE

    ############################################
    # BEGIN MML Algorithm
    ############################################

    while( ( iter < maxiter ) & ( ( globconv1 > globconv) | ( conv1 > conv) ) ){

z0 <- Sys.time()

        #****
        # collect old parameters
        b0 <- b
        a0 <- a
        dev0 <- dev
        delta0 <- delta
        pi.k0 <- pi.k

        #****
        #1 calculate probabilities
        probs <- gdm_calc_prob( a=a, b=b, thetaDes=thetaDes, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD )

        #*****
        #2 calculate individual likelihood
        res.hwt <- gdm_calc_posterior( probs=probs, gwt0=gwt0, dat=dat, I=I, resp.ind.list=resp.ind.list )
        p.xi.aj <- res.hwt$hwt

        #*****
        #3 calculate posterior and marginal distributions
        res <- gdm_calc_post( pi.k=pi.k, group=group, p.xi.aj=p.xi.aj, weights=weights, G=G,
                    ind.group=ind.group, use.freqpatt=use.freqpatt )
        p.aj.xi <- res$p.aj.xi
        pi.k <- res$pi.k

        #*****
        #4 calculate expected counts
        # n.ik [ 1:TP, 1:I, 1:(K+1), 1:G ]
        res <- gdm_calc_counts( G=G, weights=weights, dat.ind=dat.ind, dat=dat, dat.resp=dat.resp,
                    p.aj.xi=p.aj.xi, K=K, n.ik=n.ik, TP=TP, I=I, group=group, dat.ind2=dat.ind2,
                    ind.group=ind.group, use.freqpatt=use.freqpatt )
        n.ik <- res$n.ik
        N.ik <- res$N.ik

        #*****
        #5 M step: b parameter estimation
        # n.ik  [1:TP,1:I,1:K,1:G]
        # probs[1:I,1:K,1:TP]
        res <- gdm_est_b( probs=probs, n.ik=n.ik, N.ik=N.ik, I=I, K=K, G=G, b=b, b.constraint=b.constraint,
                    max.increment=max.increment.b, a=a, thetaDes=thetaDes, Qmatrix=Qmatrix, TP=TP,
                    TD=TD, msteps=msteps, convM=convM, centerintercepts=centerintercepts,
                    decrease.increments=decrease.increments )
        b <- res$b
        se.b <- res$se.b
        max.increment.b <- res$max.increment.b

        #*****
        #6 M step: a parameter estimation
        if ( irtmodel=="2PL"){
            res <- gdm_est_a( probs=probs, n.ik=n.ik, N.ik=N.ik, I=I, K=K, G=G, a=a, a.constraint=a.constraint,
                        TD=TD, Qmatrix=Qmatrix, thetaDes=thetaDes, TP=TP, max.increment=max.increment.a, b=b,
                        msteps=msteps, convM=convM, centerslopes=centerslopes, decrease.increments=decrease.increments )
            a <- res$a
            se.a <- res$se.a
            max.increment.a <- res$max.increment.a
        }

        if ( irtmodel=="2PLcat"){
            res <- gdm_est_a_cat( probs=probs, n.ik=n.ik, N.ik=N.ik, I=I, K=K, G=G, a=a, a.constraint=a.constraint,
                        TD=TD, Qmatrix=Qmatrix, thetaDes=thetaDes, TP=TP, max.increment=max.increment.a, b=b, msteps=msteps,
                        convM=convM, decrease.increments=decrease.increments )
            a <- res$a
            se.a <- res$se.a
            max.increment.a <- res$max.increment.a
        }

        #*****
        #7 M step: estimate reduced skillspace
        if ( skillspace=="loglinear" ){
            res <- gdm_est_skillspace( Ngroup=Ngroup, pi.k=pi.k, Z=delta.designmatrix, G=G, delta=delta )
            pi.k <- res$pi.k
            delta <- res$delta
            covdelta <- res$covdelta
        }
        if ( skillspace=="normal" ){
            res <- gdm_est_normalskills( pi.k=pi.k, theta.k=theta.k, irtmodel=irtmodel, G=G, D=D,
                        mean.constraint=mean.constraint, Sigma.constraint=Sigma.constraint,
                        standardized.latent=standardized.latent, p.aj.xi=p.aj.xi, group=group, ind.group=ind.group,
                        weights=weights, b=b, a=a )
            pi.k <- res$pi.k
            b <- res$b
            a <- res$a
        }

        # estimate skillspace
        if ( skillspace=="est" ){
            res <- gdm_est_skillspace_traits( n.ik=n.ik, a=a, b=b, theta.k=theta.k, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD,
                        numdiff.parm=1E-3, max.increment=1, msteps=msteps, convM=convM )
            theta.k <- res$theta.k
            se.theta.k <- res$se.theta.k
            thetaDes <- theta.k
        }

        #-- PEM acceleration
        if (PEM){
            #-- collect all parameters in a list
            parmlist <- cdm_pem_inits_assign_parmlist(pem_pars=pem_pars, envir=envir)
            #-- define log-likelihood function
            ll_fct <- gdm_calc_loglikelihood
            #- extract parameters
            ll_args <- list( irtmodel=irtmodel, skillspace=skillspace, b=b, a=a, centerintercepts=centerintercepts,
                                centerslopes=centerslopes, TD=TD, Qmatrix=Qmatrix, Ngroup=Ngroup, pi.k=pi.k,
                                delta.designmatrix=delta.designmatrix, delta=delta, G=G, theta.k=theta.k, D=D,
                                mean.constraint=mean.constraint, Sigma.constraint=Sigma.constraint,
                                standardized.latent=standardized.latent, p.aj.xi=p.aj.xi, group=group,
                                ind.group=ind.group, weights=weights, thetaDes=thetaDes, I=I, K=K, gwt0=gwt0, dat=dat,
                                resp.ind.list=resp.ind.list, use.freqpatt=use.freqpatt, p.xi.aj=p.xi.aj, TP=TP )
            #-- apply general acceleration function
            res <- cdm_pem_acceleration( iter=iter, pem_parameter_index=pem_parameter_index,
                        pem_parameter_sequence=pem_parameter_sequence, pem_pars=pem_pars,
                        PEM_itermax=PEM_itermax, parmlist=parmlist, ll_fct=ll_fct, ll_args=ll_args,
                        deviance.history=deviance.history )
            #-- collect output
            PEM <- res$PEM
            pem_parameter_sequence <- res$pem_parameter_sequence
            cdm_pem_acceleration_assign_output_parameters( res_ll_fct=res$res_ll_fct,
                            vars=pem_output_vars, envir=envir, update=res$pem_update )
        }

        #*****
        #8 calculate likelihood
        res <- gdm_calc_deviance( G=G, use.freqpatt=use.freqpatt, ind.group=ind.group, p.xi.aj=p.xi.aj,
                    pi.k=pi.k, weights=weights )
        ll <- res$ll
        dev <- res$dev
        deviance.history[iter+1] <- dev

        #~~~~~~~~~~~~~~~~~~~~~~~~~~
        # display progress
        a_change <- gg0 <- max(abs( a - a0 ))
        b_change <- gg1 <- max(abs( b - b0 ))
        pardiff <- max( b_change, a_change )
        deltadiff <- max( abs( pi.k - pi.k0 ))
        conv1 <- max( c(pardiff,deltadiff))
        globconv1 <- abs( dev - dev0)
        iter <- iter + 1

        res <- gdm_progress_em_algorithm( progress=progress, disp=disp, iter=iter, dev=dev, dev0=dev0, b_change=b_change,
                    a_change=a_change, deltadiff=deltadiff )
    }
    ############################################
    # END MML Algorithm
    ############################################

    # collect item parameters
    res <- gdm_collect_itempars( data=data, K=K, D=D, b=b, a=a, TD=TD, thetaDes=thetaDes, irtmodel=irtmodel, se.b=se.b,
                se.a=se.a, data0=data0 )
    item <- res$item
    b <- res$b
    se.b <- res$se.b
    a <- res$a

    # calculate distribution properties
    res <- gdm_calc_distributionmoments( D=D, G=G, pi.k=pi.k, theta.k=theta.k )
    mean.trait <- res$mean.trait
    sd.trait <- res$sd.trait
    skewness.trait <- res$skewness.trait
    correlation.trait <- res$correlation.trait

    # Information criteria
    ic <- gdm_calc_ic( dev=dev, dat=dat, G=G, skillspace=skillspace, irtmodel=irtmodel, K=K, D=D, TD=TD, I=I,
                b.constraint=b.constraint, a.constraint=a.constraint, mean.constraint=mean.constraint,
                Sigma.constraint=Sigma.constraint, delta.designmatrix=delta.designmatrix,
                standardized.latent=standardized.latent, data0=data0, centerslopes=centerslopes, TP=TP,
                centerintercepts=centerintercepts, centered.latent=centered.latent )

    #########################################
    # item fit [ items, theta, categories ]
    # # n.ik [ 1:TP, 1:I, 1:(K+1), 1:G ]
    probs <- aperm( probs, c(3,1,2) )
    itemfit.rmsea <- itemfit.rmsea( n.ik, pi.k, probs, itemnames=colnames(data) )
    item$itemfit.rmsea <- itemfit.rmsea$rmsea
    rownames(item) <- NULL

    # person parameters
    res <- gdm_person_parameters( data=data, D=D, theta.k=theta.k, p.xi.aj=p.xi.aj, p.aj.xi=p.aj.xi, weights=weights )
    person <- res$person
    EAP.rel <- res$EAP.rel

    #*************************
    # collect output
    s2 <- Sys.time()
    res <- list( item=item, person=person, EAP.rel=EAP.rel,
                deviance=dev, ic=ic, b=b, se.b=se.b,
                a=a,  se.a=se.a,
                itemfit.rmsea=itemfit.rmsea,
                mean.rmsea=mean(itemfit.rmsea$rmsea),
                Qmatrix=Qmatrix, pi.k=pi.k,
                mean.trait=mean.trait, sd.trait=sd.trait,
                skewness.trait=skewness.trait, correlation.trait=correlation.trait,
                pjk=probs, n.ik=n.ik,  delta.designmatrix=delta.designmatrix,
                G=G, D=D, I=ncol(data), N=nrow(data),
                delta=delta, covdelta=covdelta, data=data,
                group.stat=group.stat )
    res$p.xi.aj <- p.xi.aj ; res$posterior <- p.aj.xi
    res$skill.levels <- skill.levels
    res$K.item <- K.item
    res$theta.k <- theta.k
    res$thetaDes <- thetaDes
    res$se.theta.k <- NULL
    res$group <- group
    res$time <- list( s1=s1,s2=s2, timediff=s2-s1)
    res$skillspace <- skillspace
    res$iter <- iter
    res$converged <- iter < maxiter

    # some further values for modelfit.gdm
    res$AIC <- res$ic$AIC
    res$BIC <- res$ic$BIC
    res$Npars <- res$ic$np
    res$loglike <- - res$deviance / 2
    res$irtmodel <- irtmodel
    res$deviance.history <- deviance.history
    # control arguments
    res$control$weights <- weights
    res$control$group <- group

    if (progress){
        cat("----------------------------------- \n")
        cat("Start:", paste( s1), "\n")
        cat("End:", paste(s2), "\n")
        cat("Difference:", print(s2 -s1), "\n")
        cat("----------------------------------- \n")
    }
    class(res) <- "gdm"
    res$call <- cl
    return(res)
}
###################################################

# z0 <- cdm_timecat(z0, label=" *** gdm_calc_prob", timecat)
