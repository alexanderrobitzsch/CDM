## File Name: cdm_pem_acceleration.R
## File Version: 0.12


cdm_pem_acceleration <- function( iter, pem_parameter_index, pem_parameter_sequence,
        pem_pars, PEM_itermax, parmlist, ll_fct, ll_args, deviance.history=NULL )
{
    res0 <- ll <- NULL
    PEM <- TRUE

    #-- transform into a vector
    pem_parm <- cdm_pem_collect_parameters( parmlist=parmlist, pem_parameter_index=pem_parameter_index )
    #-- collect parameters in initial iterations
    pem_parameter_sequence <- cdm_pem_parameter_sequence_initial_iterations( pem_parm=pem_parm,
                                            pem_parameter_sequence=pem_parameter_sequence, iter=iter )
    pem_update <- FALSE

    if ( ( iter %% 2==0 ) & ( iter > 0 ) & ( iter < PEM_itermax ) ){

        pem_update <- TRUE
        pem_parameter_sequence$P2 <- pem_parm

        #** baseline likelihood
        ll_args <- cdm_pem_include_ll_args( ll_args=ll_args, pem_parm=pem_parm, pem_pars=pem_pars,
                            pem_parameter_index=pem_parameter_index )
        res0 <- res <- do.call( what=ll_fct, args=ll_args )
        ll0 <- ll <- res$ll

        P0 <- pem_parameter_sequence$P0
        P1 <- pem_parameter_sequence$P1
        P2 <- pem_parameter_sequence$P2
        iterate <- TRUE
        ii <- 0

        #--- begin PEM iterations
        while (iterate){
            ll_args0 <- ll_args
            res0 <- res
            ll0 <- ll
            tt <- cdm_pem_algorithm_compute_t( i=ii )
            Pnew <- cdm_pem_algorithm_compute_Pnew( tt=tt, P0=P0, P1=P1, P2=P2 )
            ll_args <- cdm_pem_include_ll_args( ll_args=ll_args, pem_parm=Pnew, pem_pars=pem_pars,
                            pem_parameter_index=pem_parameter_index )
            res <- do.call( what=ll_fct, args=ll_args )
            ll <- res$ll
            if ( is.na(ll) ){
                ll <- -Inf
            }
            if ( ll < ll0 ){
                iterate <- FALSE
            }
            ii <- ii + 1
        }
        #--- end PEM iterations
        ll <- res0$ll
        pem_parameter_sequence$P0 <- P1
        pem_parameter_sequence$P1 <- P2
    }
    if (iter > PEM_itermax){
        PEM <- FALSE
    }
    if ( ! is.null( deviance.history) ){
        diff_history <- diff( deviance.history[ 1:iter ] )
        NL0 <- 15
        NL <- min( NL0, iter )   # number of lags
        if ( iter > NL0 ){
            diff2 <- diff_history[ seq( iter - 1, iter - NL, -1 ) ]
            PEM <- ! ( sum( ( diff2 < 0 ) ) > ( .35 * NL0 ) )
        }

    }

    #--- output
    res <- list(ll=ll, pem_parameter_sequence=pem_parameter_sequence, PEM=PEM,
                    res_ll_fct=res0, pem_update=pem_update )
    return(res)
}

