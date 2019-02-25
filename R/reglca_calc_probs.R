## File Name: reglca_calc_probs.R
## File Version: 0.17

reglca_calc_probs <- function(parm, eps=1E-10)
{
    probs <- cumsum(parm)
    M1 <- max(probs)
    M0 <- min(probs)
    if ( ( M1>=1-eps ) | ( M0<=eps ) ){
        M1a <- max(M1, 1)
        M0a <- min(M0, 0)
        probs <- ( probs - M0a + eps / 2 ) / ( M1a - M0a + eps )
    }
    return(probs)
}
