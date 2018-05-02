## File Name: IRT.predict.R
## File Version: 1.24


########################################################################
# function for predicting item responses based on posterior
IRT.predict <- function( object , dat , group=1 )
{

    resp <- as.matrix(dat)
    # post1 <- IRT.posterior( object )
    irf1 <- IRT.irfprob( object )
    irf1[ is.na(irf1) ] <- 0
    N <- nrow(resp)
    I <- ncol(resp)
    TP <- dim(irf1)[3]
    K <- dim(irf1)[2]
    if ( length( dim(irf1) ) == 4 ){
        # handle case with group-wise item response functions
        irf1 <- irf1[ ,,,group]
    }
    irf1_ <- as.numeric(irf1)
    # call Rcpp function
    res0 <- cdm_rcpp_irt_predict( resp=resp, irf1=irf1_, K=K, TP=TP )
    probs.categ <- array( res0$probs_categ , dim=c(N,K,TP,I) )
    pred <- res0$pred
    var1 <- res0$var1
    resid1 <- res0$resid1
    sresid1 <- res0$sresid1
    # output
    res <- list( "expected" = pred , "probs.categ" = probs.categ ,
                "variance" = var1 , "residuals" = resid1 , "stand.resid" = sresid1 )
    return(res)
}
#######################################################################


# cat("output\n") ; a1 <- Sys.time() ; print( a1- a0 )    ; a0 <- a1

