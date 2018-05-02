## File Name: gdina_proc_delta_indices.R
## File Version: 0.02

gdina_proc_delta_indices <- function(delta, Mj)
{
    I <- length(delta)
    dfr <- NULL
    NP <- 0
    delta_indices <- list()
    for (ii in 1:I){
        v1 <- delta[[ii]]
        Mj_ii <- Mj[[ii]][[2]]
        NV <- length(v1)
        g1 <- NP + 1:NV
        delta_indices[[ ii ]] <- g1
        dfr1 <- data.frame("item" = ii , "np_item" = length(v1),
                    "combi" = Mj_ii , "index" = g1 , "val" = v1 )
        dfr <- rbind( dfr, dfr1 )
        NP <- NP + NV
    }
    h1 <- strsplit( paste(dfr$combi) , split="-" )
    dfr$order <- unlist( lapply( h1 , FUN = function(hh){ length(hh) } ) )
    dfr[ paste(dfr$combi) == "0" , "order" ] <- 0
    #---- output
    res <- list( delta_indices = delta_indices , delta_partable = dfr )
    return(res)
}
