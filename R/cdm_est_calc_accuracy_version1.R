## File Name: cdm_est_calc_accuracy_version1.R
## File Version: 0.047


cdm_est_calc_accuracy_version1 <- function( cdmobj, n.sims=0 )
{
    # original data
    data <- cdmobj$data
    # likelihood
    p.xi.aj <- cdmobj$like
    # class probabilities
    class.prob <- cdmobj$attribute.patt$class.prob
    # MLE (orginal data)
    c1 <- cdm_est_calc_accuracy_version1_computation( p.xi.aj=p.xi.aj,
                        est.class=cdmobj$pattern$mle.est,
                        class.prob=class.prob)

    # MAP (original data)
    c2 <- cdm_est_calc_accuracy_version1_computation( p.xi.aj=p.xi.aj,
                    est.class=cdmobj$pattern$map.est,
                    class.prob=class.prob)
    dfr <- rbind( c1, c2 )
    rownames(dfr) <- c('MLE', 'MAP' )
    if ( inherits(cdmobj,'gdina') ){
        n.sims <- 0
    }

    #--- simulated classification
    if ( sum(is.na(data) ) > 0 & ( n.sims > 0 ) ){
        n.sims <- nrow(data)
    }
    if ( n.sims > 0 ){
        I <- ncol(data)
        # splitted attribute pattern
        attr.splitted <- cdmobj$attribute.patt.splitted
        # attribute classes
        ac <- cdmobj$attribute.patt
        rownames(attr.splitted) <- rownames(ac)
        class.prob <- ac$class.prob
        CC <- nrow(ac)
        # sample patterns
        classes.sim <- sample( 1L:CC, prob=class.prob, size=n.sims, replace=TRUE)
        alpha.sim <- attr.splitted[ classes.sim,,drop=FALSE]
        # simulate according to the din model
        guess0 <- cdmobj$guess[,1]
        slip0 <- cdmobj$slip[,1]

        # simulated data (first data set)
        simdata1 <- sim.din(N=n.sims, q.matrix=cdmobj$q.matrix,
                    guess=guess0, slip=slip0,  rule=cdmobj$rule,alpha=alpha.sim)

        # simulated data (second data set)
        simdata2 <- sim.din(N=n.sims, q.matrix=cdmobj$q.matrix,
                    guess=guess0, slip=slip0,  rule=cdmobj$rule,alpha=alpha.sim)
        # handle missings
        d1 <- simdata1$dat
        if ( sum( is.na( data) ) > 0 ){ d1[ is.na(data) ] <- NA    }
        simdata1$dat <- d1
        d1 <- simdata2$dat
        if ( sum( is.na( data) ) > 0 ){ d1[ is.na(data) ] <- NA    }
        simdata2$dat <- d1
        # classification of students according
        # fixed item parameters
#        I <- length(guess0)
        mod1 <- din( simdata1$dat, q.matrix=cdmobj$q.matrix,
                        constraint.guess=cbind(1L:I, guess0),
                        constraint.slip=cbind(1L:I, slip0 ),
                        rule=cdmobj$rule, progress=FALSE, skillclasses=attr.splitted,
                        zeroprob.skillclasses=cdmobj$zeroprob.skillclasses )
        mod2 <- din( simdata2$dat, q.matrix=cdmobj$q.matrix,
                        constraint.guess=cbind(1L:I, guess0),
                        constraint.slip=cbind(1L:I, slip0 ),
                        rule=cdmobj$rule, progress=FALSE, skillclasses=attr.splitted,
                        zeroprob.skillclasses=cdmobj$zeroprob.skillclasses)
        # compare MLE estimates
        dfr1 <- data.frame( 'true'=paste( rownames(alpha.sim)  ) )
        dfr1$mle.simdata1 <- paste( mod1$pattern$mle.est )
        dfr1$mle.simdata2 <- paste( mod2$pattern$mle.est )
        # classification consistency
        dfr11 <- data.frame( 'P_c_sim'=mean( dfr1[,2]==dfr1[,3]  ) )
        dfr11$P_a_sim <-  mean( dfr1[,1]==dfr1[,3]  )
        # compare MAP estimates
        dfr1 <- data.frame( 'true'=paste( rownames(alpha.sim)  ) )
        dfr1$mle.simdata1 <- paste( mod1$pattern$map.est )
        dfr1$mle.simdata2 <- paste( mod2$pattern$map.est )
        dfr12 <- data.frame( 'P_c_sim'=mean( dfr1[,2]==dfr1[,3]  ) )
        # accuracy
        dfr12$P_a_sim <-  mean( dfr1[,1]==dfr1[,3]  )
        dfr11 <- rbind( dfr11, dfr12 )
        rownames(dfr11) <- c( 'MLE', 'MAP' )
        dfr <- cbind( dfr, dfr11 )
    }

    #********************************************
    # marginal classification
    # likelihood
    p.xi.aj <- cdmobj$like
    # splitted attribute pattern
    attribute.patt.splitted <- cdmobj$attribute.patt.splitted
    # number of skills
    K <- ncol( attribute.patt.splitted )
    dfr10 <- NULL
    for (kk in 1L:K){
        # compute marginal likelihood distribution
        ind.kk0 <- which( attribute.patt.splitted[, kk ]==0 )
        ind.kk1 <- which( attribute.patt.splitted[, kk ]==1 )
        # marginal likelihood
        pxiaj_kk <- cbind( rowSums( p.xi.aj[, ind.kk0 ] ), rowSums( p.xi.aj[, ind.kk1 ]))
        colnames(pxiaj_kk) <- c('0', '1' )
        patt1 <- 1 * ( cdmobj$pattern[, paste0( 'post.attr', kk ) ] > .5 )
        c1 <- cdmobj$skill.patt[kk,1]
        class.prob.kk <- c( 1-c1, c1 )
        c2 <- cdm_est_calc_accuracy_version1_computation( p.xi.aj=pxiaj_kk,
                        est.class=patt1, class.prob=class.prob.kk)
        dfr10 <- rbind( dfr10, c2 )
    }
    rownames(dfr10) <- paste0( 'MAP_Skill', 1L:K)
    if (n.sims>0){ dfr10 <- cbind( dfr10, NA, NA ) }
    colnames(dfr10) <- colnames(dfr)
    dfr <- rbind( dfr, dfr10 )
    dfr <- dfr[, order( paste(colnames(dfr))) ]
    return(dfr)
}


