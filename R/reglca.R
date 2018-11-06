## File Name: reglca.R
## File Version: 0.817

reglca <- function( dat, nclasses, weights=NULL, group=NULL, regular_type="scad", regular_lam=0,
                sd_noise_init=1, item_probs_init=NULL, class_probs_init=NULL, random_starts=1,
                random_iter=20,    conv=1E-5,     h=1E-4, mstep_iter=10, maxit=1000, verbose=TRUE,
                prob_min=.0001)
{

    CALL <- match.call()
    s1 <- Sys.time()

    #--- data processing
    TP <- nclasses
    res <- reglca_proc_data( dat=dat, weights=weights, group=group )
    dat <- res$dat
    dat_resp <- res$dat_resp
    resp.ind.list <- res$resp.ind.list
    dat0 <- res$dat0
    I <- res$I
    weights <- res$weights
    N <- res$N
    W <- res$W
    K <- res$K
    dat.ind2 <- res$dat.ind2
    G <- res$G
    group <- res$group
    groups_unique <- res$groups_unique
    ind_groups <- res$ind_groups
    N_groups <- res$N_groups

    #--- initial probabilities
    res <- reglca_init_parameters( nclasses=nclasses, dat0=dat0, sd_noise_init=sd_noise_init,
                    item_probs_init=item_probs_init, class_probs_init=class_probs_init,
                    random_starts=random_starts, G=G)
    class_probs_random_starts <- class_probs <- res$class_probs
    item_probs_random_starts <- item_probs <- res$item_probs
    random_starts <- res$random_starts
    use_random_starts <- res$use_random_starts
    alpha <- alpha_init <- 1
    use_dpm <- FALSE

    #--- object random starts
    control_random_starts <- list( random_starts=random_starts,    opt_fct=rep(NA, random_starts),
                                item_probs=list(), class_probs=list(), random_iter=random_iter,
                                random_start_temp=1, use_random_starts=use_random_starts )
    if ( use_random_starts ){
        item_probs <- item_probs_random_starts[[1]]
        class_probs <- class_probs_random_starts[[1]]
    }

    #--- settings
    cd_steps <- mstep_iter
    max_increment <- max_increment0 <- .2
    devchange <- 1E10
    opt_fct <- like.new <- loglike <- -1E300
    iter <- 1
    iterate <- TRUE

    #*************** EM algorithm ************************
    while (iterate){

# z0 <- Sys.time()

        item_probs0 <- item_probs
        class_probs0 <- class_probs
        loglikeold <- like.new

        #--- arrange probabilities
        pjM <- array( 0, dim=c(I,2,nclasses) )
        pjM[,2,] <- item_probs
        pjM[,1,] <- 1 - item_probs

        #--- calculate individual likelihood
        p.xi.aj <- reglca_calc_individual_likelihood( N=N, nclasses=nclasses, pjM=pjM, dat=dat, I=I,
                            resp.ind.list=resp.ind.list )

        #--- calculate posterior
        res <- reglca_calc_individual_posterior( class_probs=class_probs, p.xi.aj=p.xi.aj, N=N,
                        nclasses=nclasses, weights=weights, W=W, G=G, ind_groups=ind_groups,
                        N_groups=N_groups )
        p.aj.xi <- res$p.aj.xi
        class_probs <- res$class_probs

        #--- smoothing with Dirichlet process mixture
        if (use_dpm){
            res <- reglca_dpm_smoothing( p.aj.xi=p.aj.xi, weights=weights, nclasses=nclasses, alpha=alpha )
            alpha <- res$alpha
            class_probs <- res$class_probs
        }

        #--- calculate expected counts
        res <- reglca_calc_counts( weights=weights, dat=dat, dat.resp=dat_resp, p.aj.xi=p.aj.xi, K=K,
                            n.ik=n.ik, TP=TP, I=I, dat.ind2=dat.ind2, ind_groups=ind_groups, G=G )
        n.ik <- res$n.ik
        N.ik <- res$N.ik

        #--- item parameter estimation
        res <- reglca_mstep_item_parameters( I=I, n.ik=n.ik, N.ik=N.ik, h=h, mstep_iter=mstep_iter,
                    conv=conv, regular_lam=regular_lam, regular_type=regular_type, cd_steps=cd_steps,
                    item_probs=item_probs, max_increment=max_increment, iter=iter, G=G,
                    prob_min=prob_min)
        item_probs <- res$item_probs
        penalty <- res$penalty
        n_par <- res$n_par
        n_reg <- res$n_reg
        max_increment <- res$max_increment
        n_reg_item <- res$n_reg_item
        opt_fct_item_sum <- res$opt_fct_item_sum
        max.par.change <- max( max( abs(item_probs - item_probs0) ), max( abs( class_probs - class_probs0) ) )

        #--- calculate deviance
        res <- reglca_calc_deviance( p.xi.aj=p.xi.aj, class_probs=class_probs, weights=weights,
                    loglike=loglike, penalty=penalty, opt_fct=opt_fct, ind_groups=ind_groups, G=G,
                    N_groups=N_groups)
        like.new <- res$like.new
        likediff <- res$likediff
        opt_fct <- res$opt_fct
        opt_fct_change <- res$opt_fct_change
        loglike <- like.new

        #--- display progress
        res <- reglca_progress_em_algorithm( like.new=like.new, loglikeold=loglikeold,
                    max.par.change=max.par.change, iter=iter, progress=verbose, penalty=penalty,
                    opt_fct=opt_fct, opt_fct_change=opt_fct_change, n_reg=n_reg,
                    control_random_starts=control_random_starts, opt_fct_item_sum=opt_fct_item_sum )

        utils::flush.console()
        iter <- iter + 1 # new iteration number
        devchange <- abs( 2*(like.new-loglikeold) )

        if ( max.par.change < conv ){ iterate <- FALSE }
        if ( devchange < conv ){ iterate <- FALSE }
        if ( iter > maxit ){ iterate <- FALSE }

        #--- handle random starts
        res <- reglca_monitor_random_starts( control_random_starts=control_random_starts, iter=iter, opt_fct=opt_fct,
                    item_probs=item_probs, class_probs=class_probs, max_increment0=max_increment0,
                    max_increment=max_increment, item_probs_random_starts=item_probs_random_starts,
                    class_probs_random_starts=class_probs_random_starts )
        control_random_starts <- res$control_random_starts
        max_increment <- res$max_increment
        iter <- res$iter
        item_probs <- res$item_probs
        class_probs <- res$class_probs
        if (control_random_starts$use_random_starts){ iterate <- TRUE }

    }
    #******* end EM algorithm

    #--- information algorithm
    iter <- iter - 1
    converged <- ( iter < maxit )

    #--- process results
    rownames(item_probs) <- colnames(dat)
    colnames(item_probs) <- paste0("Class",1:nclasses)
    if (G==1){
        names(class_probs) <- colnames(item_probs)
    } else {
        rownames(class_probs) <- colnames(item_probs)
        colnames(class_probs) <- paste0("Group_", groups_unique)
    }
    item <- data.frame("item"=colnames(dat), "n_reg"=n_reg_item, item_probs )

    nd <- dim(n.ik)
    if (G==1){
        n.ik <- array(n.ik, dim=c(nd,1) )
    }

    #--- information criteria
    res <- reglca_calc_ic( loglike=loglike, nclasses=nclasses, I=I, N=N, n_reg=n_reg, G=G )
    Npars <- res$Npars
    AIC <- res$AIC
    BIC <- res$BIC
    CAIC <- res$CAIC
    Nskillpar <- res$Nskillpar
    Nipar <- res$Nipar
    deviance <- res$deviance
    ic <- res$ic

    #--- output
    time <- list(s1=s1, s2=Sys.time() )
    res <- list( item_probs=item_probs, class_probs=class_probs, p.aj.xi=p.aj.xi, p.xi.aj=p.xi.aj,
                    loglike=-deviance/2, deviance=deviance, AIC=AIC, BIC=BIC, CAIC=CAIC,
                    Npars=Npars, Nskillpar=Nskillpar, G=G, group=group, groups_unique=groups_unique,
                    N_groups=N_groups, n.ik=n.ik, Nipar=Nipar, n_reg=n_reg, n_reg_item=n_reg_item, item=item, pjk=pjM,
                    regular_type=regular_type, regular_lam=regular_lam, alpha=alpha,
                    penalty=- penalty, opt_fct=opt_fct, dat0=dat0, dat=dat, dat.resp=dat_resp,
                    weights=weights, N=N, W=W, I=I, nclasses=nclasses,
                    iter=iter, maxit=maxit, converged=converged,
                    time=time, call=CALL, ic=ic )
    class(res) <- "reglca"
    return(res)
}

# cat("calc_like ") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1
