## File Name: reduced_skillspace_beta_2_probs.R
## File Version: 0.01

reduced_skillspace_beta_2_probs <- function( Z, beta )
{
    res <- cdm_sumnorm( exp( Z %*% beta )[,1] )
    return(res)
}
