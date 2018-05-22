## File Name: slca_calc_prob0.R
## File Version: 0.04

slca_calc_prob0 <- function( Xdes, Xlambda, I,K,TP)
{
    # Xdes [ 1:I, 1:(K+1), 1:TP, 1:Nlam ]
    p1 <- probs <- array( 0, dim=c(I,K+1,TP) )
    for (tt in 1:TP){
        tmp0 <- 0
        for (hh in 1:(K+1) ){
            tmp1 <- exp( Xdes[, hh, tt, ] %*% Xlambda )
            tmp0 <- tmp0 + tmp1
            p1[, hh, tt  ] <- tmp1
        }
        for (hh in 1:(K+1) ){
            probs[,hh,tt] <- p1[, hh, tt  ] / tmp0
        }
    }
    return(probs)
}

.slca.calc.prob0 <- slca_calc_prob0

