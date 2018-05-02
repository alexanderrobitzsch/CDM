## File Name: gdm_thetadesign.R
## File Version: 0.02


##########################################
# Theta design matrix
gdm_thetadesign <- function( theta.k , thetaDes , Qmatrix )
{
    D <- 1  # default dimension 1
    ####################
    # definition of theta.k
    if ( ! is.null(Qmatrix) ){
        D <- ncol(Qmatrix)
        if ( length( dim(Qmatrix))==2 ){
            Q1 <- array( 0 , dim=c(dim(Qmatrix),1) )
            Q1[,,1] <- Qmatrix
            Qmatrix <- Q1
        }
    }
    w1 <- ( is.vector( theta.k)  ) & ( ! is.list( theta.k) )
    if ( w1 ){
        theta.k <- matrix( theta.k , ncol=1 )
        if (D>1){
            th1 <- as.list(1:D)
            for (dd in 1:D){
                th1[[dd]] <- theta.k
            }
            theta.k <- th1
        }
    }
    if ( is.list( theta.k) ){
        tk <- theta.k
        theta.k <- expand.grid( theta.k )
        colnames(theta.k) <- names(tk)
    }
    theta.k <- as.matrix(theta.k)
    D <- ncol(theta.k)
    if ( is.null( colnames(theta.k) ) ){
        colnames(theta.k) <- paste0("F",1:D)
    }
    ##############################
    if ( is.null(thetaDes) ){
        # thetaDes [TP,TD]
        TD <- D
        thetaDes <- matrix( theta.k , ncol=TD )
        colnames(theta.k) -> colnames(thetaDes)
    }
    TP <- nrow(thetaDes)
    TD <- ncol(thetaDes)
    res <- list(D=D , TD=TD , TP=TP,theta.k=theta.k,
            thetaDes=thetaDes , Qmatrix=Qmatrix )
    return(res)
}
