## File Name: mcdina_proc_modify_qmatrix.R
## File Version: 0.02


# modify q-matrix
mcdina_proc_modify_qmatrix <- function( q.matrix, skillclasses)
{
    # create new q.matrix
    K <- ncol(q.matrix) - 2
    maxattr <- apply( q.matrix[,-c(1:2) ], 2, max )
    qmatrix_mod <- NULL
    q.matrix1 <- q.matrix[,1:2]
    K1 <- max(maxattr)
    res <- list( "q.matrix"=q.matrix, "q.matrix0"=NULL,
            "maxmaxattr"=K1, "skillclasses"=skillclasses,
            "skillclasses0"=skillclasses, "qmatrix_mod"=NULL )
    if (K1 > 1 ){
        m1 <- matrix( 0:K1, nrow=K1+1, ncol=K )
        skillclasses <- as.matrix( expand.grid( as.data.frame( m1) ) )
        colnames(skillclasses) <- colnames(q.matrix)[ -c(1:2) ]
        # create modified q-matrix
        for (kk in 1:K){ # kk <- 1
            qmatrix_mod.kk <- data.frame( "attr_index"=kk,
                "maxattr"=maxattr[kk] )
            skillclasses <- skillclasses[ skillclasses[,kk] <=maxattr[kk], ]
            for (zz in 1:(maxattr[kk] ) ){ #     zz <- 1
                name <- paste0( colnames(q.matrix)[kk+2], ".L", zz )
                q.matrix1[,  name ] <- 1 * ( q.matrix[, kk + 2] >=zz )
            }
            qmatrix_mod <- rbind( qmatrix_mod, qmatrix_mod.kk )
        }
        qmatrix_mod$start <- c(1,cumsum( qmatrix_mod$maxattr)[ - K ] + 1  )
        qmatrix_mod$end <- cumsum( qmatrix_mod$maxattr)
        skillclasses0 <- skillclasses
        rownames(skillclasses0) <- cdm_matrixstring( skillclasses0, "P" )
        skillclasses <- as.data.frame(skillclasses)
        # create modified skillclasses
        for (kk in 1:K){ # kk <- 1
            for (zz in 1:(maxattr[kk] ) ){ #     zz <- 1
                name <- paste0( colnames(q.matrix)[kk+2], ".L", zz )
                skillclasses[,  name ] <- 1 * ( skillclasses[, kk ] >=zz )
            }
        }
        skillclasses <- skillclasses[, - c(1:K) ]
        rownames(skillclasses) <- cdm_matrixstring( skillclasses, "P" )
        res$q.matrix <- q.matrix1
        res$skillclasses <- as.matrix(skillclasses)
        res$skillclasses0 <- skillclasses0
        res$q.matrix0 <- q.matrix
        res$qmatrix_mod <- qmatrix_mod
    }
    return(res)
}
