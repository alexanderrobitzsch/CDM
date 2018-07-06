## File Name: mcdina_proc_item_latent_response.R
## File Version: 0.08


##############################################
# compute preparation table for one item
mcdina_proc_item_latent_response <- function( ii, q.matrix, K, TP, skillclasses, classes )
{
    q.ii <- q.matrix[ q.matrix[,1]==ii, ]
    # categories
    cats.ii <- q.ii[,2]
    CC <- length(cats.ii)
    # calculate relevant attributes
    qsum <- rowSums( q.ii[, 1:K + 2  ]  )
    index.max <- which( qsum==max(qsum) )
    # necessary attributes for item ii
    attr.ii <- which( q.ii[ index.max[1], 1:K + 2] > 0 )
    if ( length(attr.ii)==0 ){
        attr.ii <- 1:K
    }
    q.ii.red <- q.ii[, attr.ii + 2, drop=FALSE]
    # calculate matrix with skill classes
    sk.ii1 <- sk.ii2 <- matrix( 0, nrow=TP, ncol=CC)
    colnames(sk.ii1) <- colnames(sk.ii2) <- paste0("Cat", cats.ii )
    rownames(sk.ii1) <- rownames(sk.ii2) <- rownames(skillclasses)
    for (cc in 1:CC){
        sk.ii2[, cc] <- 1 * ( rowSums( skillclasses[, attr.ii, drop=FALSE] !=q.ii.red[rep(cc,TP),] )==0 )
        tmp1 <- skillclasses[, attr.ii, drop=FALSE] %*% t( q.ii.red[cc,]  )
        sk.ii1[, cc] <- 1 * ( tmp1 >=sum( q.ii.red[cc,] ) )
        sk.ii1[, cc] <-  tmp1*sk.ii1[, cc]
    }
    sk.ii1 <- 1 * ( sk.ii1 > 0 )
    v1.ii <- which( rowSums( sk.ii1 )==0 )
    i5 <- which( rowSums( q.ii.red )==0 )
    sk.ii1[ v1.ii, i5 ] <- 1
    ind.ii <- which( rowSums( sk.ii2 )==0 )
    sk.ii2[ind.ii, ] <- sk.ii1[ ind.ii, ]
    # define latent response groups
    lg <- "LR"
    for (cc in 1:CC){
        lg <- paste0( lg, ifelse( sk.ii2[,cc]==1, cats.ii[cc], "") )
    }
    groups <- sort( unique(lg) )
    lr <- data.frame("item"=ii, "skillclass"=classes,
        "skillclass_index"=1:TP, "lr"=lg )
    lr$lr_index <- match( lr$lr, groups )
    # unique latent groups
    lg1 <- sapply( cats.ii, FUN=function(cc){ grep( cc, groups) } )
    lc <- data.frame("item"=ii, "cats"=cats.ii,
                "lr"=groups[ lg1 ] )

    lc$max.cat <- 0
    lc$max.cat[ index.max ] <- 1
    lc$lr_index <- match( lc$lr, groups )
    lc$Q <- cdm_matrixstring( q.ii[, 1:K + 2  ], "Q" )
    lc$lr_level <- rowSums( q.ii[, 1:K + 2  ])
    lc <- lc[ order( paste( lc$lr_level, lc$cats) ), ]
    lc$lr_level <- paste0( lc$lr_level,
        LETTERS[ match( lc$lr, unique(lc$lr) ) ] )
    lc <- lc[ order( paste( lc$cats) ), ]
    # item statistics
    itemstat <- data.frame("item"=ii, "N.cat"=CC,
            "N.lr"=length(groups) )
    itemstat$N.attr <- length(attr.ii)
    res <- list("lr"=lr, "lc"=lc, "itemstat"=itemstat)
    return(res)
}
