## File Name: reglca_normalize_probabilities.R
## File Version: 0.04

reglca_normalize_probabilities <- function(parm)
{
    eps <- 1E-10
    probs <- cumsum(parm)
    M <- max(probs)
    if (M > 1){
        probs <- probs / ( M + eps )
        parm <- c( probs[1], diff(probs) )
    }
    return(parm)
}
