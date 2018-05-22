## File Name: modelfit.cor2.R
## File Version: 3.80



modelfit.cor2 <- function( data, posterior, probs )
{
# z0 <- Sys.time()
    K <- max( apply( data, 2, max, na.rm=TRUE ) )
    if ( K>1 ){
        stop("modelfit.cor only allows for dichotomous data\n")
    }
    data <- as.matrix(data)
    I <- ncol(data)
    posterior <- as.matrix(posterior)
# cat("start ") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1

    data_na <- is.na(data)
    data.resp <- 1 - data_na
    data[ data_na ] <- 9
    data1 <- data
    data1[ data_na ] <- 0
    data_resp_bool <- ! data_na

    res <- cdm_rcpp_modelfit_cor_counts( data=data, data_resp_bool=data_resp_bool)
    n11 <- res$n11
    n01 <- res$n01
    n10 <- res$n10
    n00 <- res$n00

    #----- covariances
    ip <- itempairs <- as.matrix( t( utils::combn(I,2 ) ) )
    colnames(itempairs) <- c("item1", "item2" )
    itempairs <- as.data.frame( itempairs )
    itempairs$n11 <- n11[ ip ]
    itempairs$n10 <- n10[ ip ]
    itempairs$n01 <- n01[ ip ]
    itempairs$n00 <- n00[ ip ]
    itempairs$n <- rowSums( itempairs[, c("n11","n10", "n01","n00") ] )

    #---- calculate expected score for every person and every item
    exp.ii.jj <- posterior %*% t( probs[,2,] )
    probs1 <- as.matrix(probs[, 2, ])
    probs0 <- as.matrix(probs[, 1, ])
    ip1 <- as.matrix(ip-1)
    res <- cdm_rcpp_modelfit_cor2( posterior=posterior, data=data,
                data_resp_bool=data_resp_bool, probs1=probs1, probs0=probs0, ip=ip1,
                expiijj=exp.ii.jj )
    r1 <- res$itempair_stat

    itempairs$Exp11 <- r1[,1]
    itempairs$Exp10 <- r1[,2]
    itempairs$Exp01 <- r1[,3]
    itempairs$Exp00 <- r1[,4]

    # observed correlation
    n <- itempairs$n
    m1 <- ( itempairs$n10 + itempairs$n11 ) / n
    m2 <- ( itempairs$n01 + itempairs$n11 ) / n
    t1 <- itempairs$n11 / n  - m1 * m2
    itempairs$corObs <- t1 / sqrt( m1 * ( 1 - m1 ) * m2 * ( 1-m2 ) )

    # observed correlation
    m1 <- ( itempairs$Exp10 + itempairs$Exp11 ) / n
    m2 <- ( itempairs$Exp01 + itempairs$Exp11 ) / n
    t1 <- itempairs$Exp11 / n  - m1 * m2
    itempairs$corExp <- t1 / sqrt( m1 * ( 1 - m1 ) * m2 * ( 1-m2 ) )

    # define further quantities
    itempairs$X2 <- NA
    itempairs$RESIDCOV <- NA
    itempairs$Q3 <- res$Q3

    ##############################
    itempairs$X2 <- ( itempairs$n00 - itempairs$Exp00 )^2 / itempairs$Exp00 +
                    ( itempairs$n10 - itempairs$Exp10 )^2 / itempairs$Exp10    +
                    ( itempairs$n01 - itempairs$Exp01 )^2 / itempairs$Exp01    +
                    ( itempairs$n11 - itempairs$Exp11 )^2 / itempairs$Exp11
    itempairs$RESIDCOV <- ( itempairs$n11 * itempairs$n00 - itempairs$n10 * itempairs$n01 ) / itempairs$n^2 -
                ( itempairs$Exp11 * itempairs$Exp00 - itempairs$Exp10 * itempairs$Exp01 ) / itempairs$n^2
    ##############################
    # labels
    itempairs$item1 <- colnames(data)[ itempairs$item1 ]
    itempairs$item2 <- colnames(data)[ itempairs$item2 ]

    #-- absolute difference in correlations
    itempairs$fcor <- cdm_fisherz( itempairs$corObs ) - cdm_fisherz( itempairs$corExp )
    itempairs <- itempairs[ itempairs$n > 0, ]

    #--- p values and p value adjustments adjustments
    # X2 statistic
    itempairs$X2_df <- 1
    itempairs$X2_p <- 1 - stats::pchisq(itempairs$X2, df=1 )
    itempairs$X2_p.holm <- stats::p.adjust( itempairs$X2_p, method="holm")
    itempairs$X2_sig.holm <- 1 * ( itempairs$X2_p.holm < .05 )
    itempairs$X2_p.fdr <- stats::p.adjust( itempairs$X2_p, method="fdr")
    # fcor statistic
    itempairs$fcor_se <- ( itempairs$n - 3 )^(-1/2)
    itempairs$fcor_z <- itempairs$fcor / itempairs$fcor_se
    itempairs$fcor_p <- 1 - stats::pnorm( abs(itempairs$fcor_z ) )
    itempairs$fcor_p.holm <- stats::p.adjust( itempairs$fcor_p, method="holm")
    itempairs$fcor_p.fdr <- stats::p.adjust( itempairs$fcor_p, method="fdr")


    #**********************
    # model fit
    modelfit <- data.frame( "est"=c(
            mean( abs( itempairs$corObs - itempairs$corExp ), na.rm=TRUE),
            sqrt( mean( ( itempairs$corObs - itempairs$corExp )^2, na.rm=TRUE ) ),
            mean( itempairs$X2 ), # mean( itempairs$G2),
            mean( 100*abs(itempairs$RESIDCOV ), na.rm=TRUE ),
            mean( abs( itempairs$Q3 ), na.rm=TRUE),
            mean( abs( itempairs$Q3 - mean(itempairs$Q3,na.rm=TRUE) ), na.rm=TRUE )
                        ) )
    rownames(modelfit) <- c("MADcor", "SRMSR", "MX2", # "MG2",
                "100*MADRESIDCOV", "MADQ3", "MADaQ3" )

    modelfit <- modelfit[ ! ( rownames(modelfit) %in% c("MX2") ),, drop=FALSE ]

    #*****
    # summary statistics
    modelfit.test <- data.frame("type"=c("max(X2)","abs(fcor)"),
            "value"=c( max( itempairs$X2), max( abs(itempairs$fcor) )  ),
            "p"=c( min( itempairs$X2_p.holm), min( itempairs$fcor_p.holm)  )
                )
    #**** statistics for use in IRT.compareModels
    statlist <- data.frame( "maxX2"=modelfit.test[1,"value"],
                    "p_maxX2"=modelfit.test[1,"p"] )
    h1 <- modelfit$est
    statlist <- cbind( statlist, t(h1 ) )
    names(statlist)[-c(1:2) ] <- rownames(modelfit)

    #--- output print results
    res <- list( "modelfit.stat"=modelfit, "itempairs"=itempairs,
        "modelfit.test"=modelfit.test, "statlist"=statlist )
    return(res)
}

