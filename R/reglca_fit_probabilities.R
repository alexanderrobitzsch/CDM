## File Name: reglca_fit_probabilities.R
## File Version: 0.26

reglca_fit_probabilities <- function( freq, lambda, parm_init=NULL, regular_type="scad",
    h=1E-4, maxit=100, conv=1E-5, cd_steps=5, max_increment=1, verbose=TRUE)
{

    #--- order frequencies
    NP <- length(freq)
    freq_index <- data.frame("index" =1:NP , "freq" = freq )
    freq_index <- freq_index[ order(freq_index$freq), ]
    freq <- freq_index$freq
    C <- freq
    W <- 1 - freq

    #--- init parameters
    if ( is.null(parm_init) ){
        parm_init <- c( freq[1], diff(freq) )
    }
    parm <- parm_init

    iter <- 0
    iterate <- TRUE

    #---- begin iterations
    while(iterate){

        res <- reglca_fit_probabilities_fit_function( parm=parm, lambda=lambda, C=C, W=W,
                            regular_type=regular_type )
        ll <- res$ll
        pen <- res$pen
        fit_fct <- res$fit_fct
        parm_old <- parm
        for (pp in 1:NP){
            parm <- reglca_update_parameter( parm=parm, pp=pp, C=C, W=W, h=h, lambda=lambda,
                        regular_type=regular_type, cd_steps=cd_steps, conv=conv,
                        max_increment=max_increment)
        }
        #-- normalize probabilities
        parm <- reglca_normalize_probabilities(parm=parm)
        parchange <- max( abs( parm - parm_old ))
        if (iter > maxit){ iterate <- FALSE }
        if (parchange < conv){ iterate <- FALSE }
        iter <- iter + 1
        if (verbose){
            cat( paste0("Iteration ", iter , " | Max. parm. change=",
                    round( parchange, 6) , " | Fit function = " , round( fit_fct , 6) , "\n") )
            utils::flush.console()
        }
    }

    freq_index$fitted <- cumsum(parm)
    probs <- rep(0,NP)
    probs[ freq_index$index ] <- freq_index$fitted
    n_par <- sum( abs(parm) > 1E-10 )

    #--- output
    res <- list( parm=parm, n_par=n_par, probs=probs, ll=ll, pen=pen, fit_fct=fit_fct)
    return(res)
}
