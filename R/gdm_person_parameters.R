## File Name: gdm_person_parameters.R
## File Version: 0.08



###########################################
# person parameter estimates
gdm_person_parameters <- function( data, D, theta.k,
    p.xi.aj, p.aj.xi, weights )
{
    #**************************
    person <- data.frame("case"=1:(nrow(data)), "M"=rowMeans( data, na.rm=T)  )
    EAP.rel <- rep(0,D)
    names(EAP.rel) <- colnames(theta.k)
    nstudl <- rep(1,nrow(data))
    doeap <- TRUE
    if ( is.list( p.aj.xi)){
        p.aj.xi <- p.aj.xi[[1]]
        nstudl <- rep(1,nrow(p.aj.xi ) )
        weights <- weights[,1]
        weights <- weights[ weights > 0 ]
        doeap <- FALSE
    }
    if (doeap ){
        for (dd in 1:D){ #dd <- 1
            dd1 <- colnames(theta.k)[dd]
            person$EAP <- rowSums( p.aj.xi * outer( nstudl, theta.k[,dd] ) )
            person$SE.EAP <- sqrt(rowSums( p.aj.xi * outer( nstudl, theta.k[,dd]^2 ) ) - person$EAP^2)
            EAP.variance <- stats::weighted.mean( person$EAP^2, weights ) -
                                ( stats::weighted.mean( person$EAP, weights ) )^2
            EAP.error <- stats::weighted.mean( person$SE.EAP^2, weights )
            EAP.rel[dd] <- EAP.variance / ( EAP.variance + EAP.error )
            colnames(person)[ which( colnames(person)=="EAP" ) ] <- paste("EAP.", dd1, sep="")
            colnames(person)[ which( colnames(person)=="SE.EAP" ) ] <- paste("SE.EAP.", dd1, sep="")
        }
        # MLE
        mle.est <- theta.k[ max.col( p.xi.aj ),, drop=FALSE]
        colnames(mle.est) <- paste0( "MLE.", names(EAP.rel))
        person <- cbind( person, mle.est )
        # MAP
        mle.est <- theta.k[ max.col( p.aj.xi ),, drop=FALSE]
        colnames(mle.est) <- paste0( "MAP.", names(EAP.rel))
        person <- cbind( person, mle.est )
    }
    #--- OUTPUT
    res <- list( person=person, EAP.rel=EAP.rel )
    return(res)
}
###########################################################################


.gdm.person.parameters <- gdm_person_parameters
