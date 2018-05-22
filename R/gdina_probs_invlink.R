## File Name: gdina_probs_invlink.R
## File Version: 0.03

gdina_probs_invlink <- function(probs, linkfct)
{
    if ( linkfct=="logit"){
        probs <- stats::plogis(probs)
    }
    if ( linkfct=="log"){
        probs <- exp(probs)
    }
    return(probs)
}
