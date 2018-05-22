## File Name: din_equivalent_class_latent_response.R
## File Version: 0.04


#**********************************************************
# calculates a latent response under the din function
din_equivalent_class_latent_response <- function( q.matrix, S, rule="DINA")
{
    Q <- as.matrix(q.matrix)
    S <- as.matrix(S)
    L <- matrix(nrow=nrow(Q), ncol=nrow(S))
    SQ <- S %*% t(Q)
    nums <- rowSums(Q)
    nums <- ifelse( rule=="DINO", 1, nums )
    nums <- matrix( nums, nrow=nrow(SQ), ncol=ncol(SQ), byrow=TRUE )
    SQ <- 1 * ( SQ >=nums  )
    L <- t(SQ)
    colnames(L) <- rownames(S)
    return(L)
}

din.latent.response <- din_equivalent_class_latent_response
