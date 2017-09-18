## File Name: jkestimates.R
## File Version: 0.05
## File Last Change: 2017-01-31 14:07:28


################################################
# jkestimates
jkestimates <- function( est , parsM , fayfac ){
    RR <- ncol(parsM)
    M_pars <- rowMeans( parsM  )
    parsMres <- parsM - M_pars
    # variance covariance matrix of estimators
    vcov_pars <- tcrossprod( parsMres )     
    dfr <- data.frame( "est" = est , 
			"jkest" = est - fayfac * (RR-1) * ( M_pars - est) )
    dfr$jkse <- sqrt( diag( vcov_pars ))
    res <- list("dfr" = dfr , "vcov_pars"=vcov_pars )
    return(res)
            }
################################################			
