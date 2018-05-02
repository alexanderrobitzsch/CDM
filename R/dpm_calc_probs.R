## File Name: dpm_calc_probs.R
## File Version: 0.02

dpm_calc_probs <- function( vh )
{
    N_max <- length(vh)
    probs <- rep( 1, N_max)
    probs[1] <- vh[1]
    for (tt in 2:N_max){
        probs[tt] <- vh[tt] * prod( 1 - vh[ 1:(tt-1)] )
    }
    return(probs)
}
