## File Name: gdina_se_itemwise.R
## File Version: 0.17

gdina_se_itemwise <- function( R.lj_jj , I.lj_jj , apjj ,
        Mjjj , Mjj2 , PAJXI , IP , item.patt.split_jj , resp.patt_jj ,
        freq.pattern , item.patt.freq, avoid.zeroprobs    , data , jj ,
        method , linkfct , delta_jj , se_version )
{
    eps2 <- 1E-10
    Rlj.ast <- stats::aggregate( R.lj_jj , list(apjj) , sum )
    Ilj.ast <- stats::aggregate( I.lj_jj , list(apjj) , sum )
    pjjj <- Rlj.ast[,2] / Ilj.ast[,2]
    varmat.palj_jj  <- NULL
    infomat.jj <- NULL

    #********* standard error calculation observed log-likelihood per item
    if (se_version==1){
        loglike_item_jj <- function(x){
            pjjj_model <- ( Mjjj %*% x )[,1]
            if ( linkfct == "logit"){
                pjjj_model <- stats::plogis(pjjj_model)
            }
            if ( linkfct == "log"){
                pjjj_model <- exp( pjjj_model)
            }
            pjjj_ <- cdm_squeeze(pjjj_model , c(eps2, Inf) )
            ll1 <- Rlj.ast[,2] * log(pjjj_)
            pjjj_ <- cdm_squeeze(1-pjjj_model , c(eps2, Inf) )
            ll2 <- (Ilj.ast-Rlj.ast)[,2] * log( pjjj_ )
            ll <- sum(ll1 + ll2)
            return( ll )
        }
        res_jj <- loglike_item_jj(x=delta_jj)
        hess_jj <- numerical_Hessian( par = delta_jj , FUN = loglike_item_jj )
        varmat.delta_jj <- MASS::ginv( - hess_jj )
    }

    #********* standard error calculation formulas de la Torre (2011)
    if (se_version==0){
        Mjjj <- Mjjj[ sort(unique(apjj)) , ]
        M1 <- length( unique(apjj) )
        p.ajast.xi <- matrix( 0 , nrow=IP , ncol = M1 )
        for (kk in 1:M1){
            pg1 <-  PAJXI[ , apjj == kk  ]
            if ( is.vector(pg1)){
                p.ajast.xi[,kk] <- pg1
            } else {
                p.ajast.xi[,kk] <- rowSums( pg1 )
            }
        }
        pjjjM <- outer( rep(1,IP) , pjjj ) + eps2
        nM <- ncol(pjjjM)
        x1 <- outer( item.patt.split_jj , rep(1,nM) )
        r1 <- outer( resp.patt_jj * item.patt.freq , rep(1,ncol(pjjjM) ) )
        # Formula (17) for calculating the standard error
        mat.jj <- p.ajast.xi * ( x1 - pjjjM) / ( pjjjM * ( 1 - pjjjM ) + eps2)

        infomat.jj <- matrix( 0 , nM , nM )
        for (kk1 in 1:nM){
            for (kk2 in kk1:nM){
                # frequency weights must be taken into account
                hh1 <- sum( mat.jj[,kk1] * mat.jj[,kk2] * freq.pattern *
                                    resp.patt_jj * item.patt.split_jj )
                infomat.jj[kk2,kk1] <- infomat.jj[kk1,kk2] <-  hh1
            }
        }
        if ( avoid.zeroprobs ){
            ind <- which( is.na(diag(infomat.jj) ))
            if ( length(ind) > 0 ){
                infomat.jj <- infomat.jj[-ind, -ind]
            }
        }
        a1 <- try( solve( infomat.jj + diag( eps2 , ncol(infomat.jj) ) ) )
        if ( is(a1 , "try-error") ){
            cat( "Item" , colnames(data)[jj] , "Singular item parameter covariance matrix\n")
            a1 <- NA*infomat.jj
        }
        varmat.palj_jj <- Ijj <- a1
        Wj <- diag( Ilj.ast[,2] )
        if ( avoid.zeroprobs ){
            ind <- which( Ilj.ast[,2]  < eps2  )
            if ( length(ind) > 0 ){
                Wj <- diag( Ilj.ast[-ind,2] )
                Mjjj <- Mjjj[ - ind , ]
                pjjj <- pjjj[ - ind  ]
            }
        }

        if ( ( method == "ULS" ) ){
            x1 <- t(Mjjj) %*% Mjjj
            diag(x1) <- diag(x1) + eps2
            Wjjj <- solve( x1 ) %*% t(Mjjj)
        } else {
            x1 <- t(Mjjj) %*% Wj %*% Mjjj
            diag(x1) <- diag(x1) + eps2
            Wjjj <- solve( x1 ) %*% t(Mjjj) %*% Wj
        }
        if ( linkfct == "logit" ){
            pjjj.link <- 1 / ( ( pjjj * ( 1 - pjjj ) ) + eps2 )
            pjjj.link <- diag( pjjj.link )
            Wjjj <- Wjjj %*% pjjj.link
        }
        if ( linkfct == "log" ){
            pjjj.link <- 1 /  ( pjjj  + eps2 )
            pjjj.link <- diag( pjjj.link )
            Wjjj <- Wjjj %*% pjjj.link
        }
        varmat.delta_jj <- Wjjj %*% Ijj %*% t(Wjjj)
    }
    #--- output
    res <- list( infomat.jj = infomat.jj , varmat.palj_jj = varmat.palj_jj ,
                    varmat.delta_jj = varmat.delta_jj)
    return(res)
}
