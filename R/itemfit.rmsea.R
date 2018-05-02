## File Name: itemfit.rmsea.R
## File Version: 0.30

###########################################################
# RMSEA Item fit
itemfit.rmsea <- function( n.ik , pi.k , probs , itemnames=NULL)
{
    # probs ... [ classes , items , categories ]
    # n.ik ... [ classes , items , categories , groups ]

    if (is.vector(pi.k)){
        pi.k <- matrix(pi.k, ncol=1)
    }

    # RMSEA (RMSD statistic) for all groups
    itemfit.rmsea <- itemfit_rmsea_helper( n.ik=n.ik, pi.k=pi.k, probs=probs )
    if ( ! is.null(itemnames) ){
        names(itemfit.rmsea) <- itemnames
    }
    # groupwise RMSEA
    G <- dim(n.ik)[4]
    I <- dim(n.ik)[2]
    rmsea.groups <- matrix( NA , I , G )
    if ( ! is.null(itemnames) ){
        rownames(rmsea.groups) <- itemnames
    }
    for (gg in 1:G){
        rmsea.groups[,gg] <- itemfit_rmsea_helper( n.ik=n.ik[,,,gg,drop=FALSE]  , pi.k=pi.k, probs=probs )
    }
    res <- list( "rmsea" = itemfit.rmsea , "rmsea.groups"=rmsea.groups )
    return(res)
}
##########################################

