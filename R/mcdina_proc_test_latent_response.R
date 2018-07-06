## File Name: mcdina_proc_test_latent_response.R
## File Version: 0.05


##########################################
# preparation function for whole test
mcdina_proc_test_latent_response <- function( q.matrix, K, TP, skillclasses, classes )
{
    I <- length( unique(q.matrix[,1]))
    lr <- NULL
    lc <- NULL
    itemstat <- NULL
    for (ii in 1:I){
        res <- mcdina_proc_item_latent_response( ii=ii, q.matrix=q.matrix, K=K, TP=TP,
                    skillclasses=skillclasses, classes=classes )
        lr <- rbind( lr, res$lr )
        lc <- rbind( lc, res$lc )
        itemstat <- rbind( itemstat, res$itemstat )
    }
    res <- list("lr"=lr, "lc"=lc, "itemstat"=itemstat)
    return(res)
}
###############################################
