## File Name: cdm_est_calc_accuracy_version2.R
## File Version: 0.153


cdm_est_calc_accuracy_version2 <- function( cdmobj, n.sims=0 )
{
    #*** simulation based measures
    irfprob <- IRT.irfprob(cdmobj)
    theta <- attr(irfprob, "theta")
    TP <- nrow(theta)
    prob_theta <- attr(irfprob, "prob.theta")
    prob_theta <- cdm_shrink_positive(x=prob_theta)
    prior <- prob_theta
    data <- IRT.data(cdmobj)
    N <- nrow(data)
    if (n.sims==0){
        n.sims <- N
    }
    sizes <- round( n.sims*prob_theta )
    theta_index <- rep(1:TP, sizes)
    K <- ncol(theta)   # number of skills
    skill_names <- colnames(theta)
    if (is.null(skill_names)){
        skill_names <- paste0("skill",1:K)
    }
    statistics_names <- c( "MLE_patt", "MAP_patt", paste0("MLE_", skill_names),
                        paste0("MAP_", skill_names) )

    #-- simulate item responses
    dat1 <- sim_model(object=NULL, irfprob=irfprob, theta_index=theta_index, data=data,
                    N_sim=n.sims )$dat
    dat2 <- sim_model(object=NULL, irfprob=irfprob, theta_index=theta_index, data=data,
                    N_sim=n.sims )$dat

    #-- compute classifications based on simulated data
    class1 <- cdm_est_calc_accuracy_version2_classify_simulated_data( data=dat1,
                    irfprob=irfprob, prior=prior)
    class2 <- cdm_est_calc_accuracy_version2_classify_simulated_data( data=dat2,
                    irfprob=irfprob, prior=prior)

    #-- compute accuracy and consistency
    dfr <- as.data.frame( matrix( 0, nrow=2*(1+K), ncol=4 ) )
    colnames(dfr) <- c("Pa_est", "Pa_sim", "Pc_est", "Pc_sim")
    rownames(dfr) <- statistics_names

    estimators <- c("MLE", "MAP")
    #-- multivariate pattern
    for (pp in estimators ){
        dfr[ paste0(pp, "_patt"), "Pa_sim" ] <- ( mean( theta_index==class1[[pp]] ) +
                                mean( theta_index==class2[[pp]] ) ) / 2
        dfr[ paste0(pp, "_patt"), "Pc_sim" ] <- mean( class1[[pp]]==class2[[pp]] )
    }
    #-- single skills
    for (kk in 1:K){
        for (pp in estimators){
            theta_index_kk <- theta[ theta_index, kk]
            est1_kk <- theta[ class1[[pp]], kk]
            est2_kk <- theta[ class2[[pp]], kk]
            dfr[ paste0(pp, "_", skill_names[kk] ), "Pa_sim" ] <-
                                ( mean( theta_index_kk==est1_kk ) +
                                    mean( theta_index_kk==est2_kk ) ) / 2
            dfr[ paste0(pp, "_", skill_names[kk] ), "Pc_sim" ] <- mean( est1_kk==est2_kk )
        }
    }

    #**** estimated statistics
    like <- IRT.likelihood(cdmobj)
    post <- IRT.posterior(cdmobj)
    eps <- 1E-7
    for (pp in estimators){
        if (pp=="MLE"){ matr <- like } else { matr <- post }
        est <- cdm_rcpp_irt_classify_individuals(like=as.matrix(matr))$class_index
        index <- cbind(1:N, est)

        #*** accuracy
        dfr[ paste0(pp, "_patt"), "Pa_est" ] <- sum( post[ index ] ) / N
        theta_est <- theta[ est, ]
        for (kk in 1:K){
            post0 <- sum( post[, theta[,kk]==0 ] * ( theta_est[, kk ]==0 ) ) / N
            post1 <- sum( post[, theta[,kk]==1 ] * ( theta_est[, kk ]==1 ) ) / N
            dfr[ paste0(pp, "_", skill_names[kk] ), "Pa_est" ] <- post0 + post1
        }
        #*** consistency
        val <- cdm_rcpp_est_calc_accuracy_version2_consistency_helper( post=post,
                    est=est-1, max_est_index=TP, N=N, prob_theta=prob_theta, eps=eps )
        dfr[ paste0(pp, "_patt"), "Pc_est" ] <- val
        for (kk in 1:K){
            est_kk <- theta[ est, kk]
            val <- cdm_rcpp_est_calc_accuracy_version2_consistency_helper( post=post,
                    est=est_kk, max_est_index=2, N=N, prob_theta=prob_theta, eps=eps )
            dfr[ paste0(pp, "_", skill_names[kk] ), "Pc_est" ] <- val
        }
    }

    #--- output
    return(dfr)
}

