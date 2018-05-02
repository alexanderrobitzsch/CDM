## File Name: gdina_reduced_skillspace_single_group.R
## File Version: 0.04

gdina_reduced_skillspace_single_group <- function( Z, reduced.skillspace.method, ipmat, post )
{
    ntheta <- colSums( ipmat * post )
    res <- gdina_reduced_skillspace( ntheta=ntheta, Z=Z, reduced.skillspace.method=reduced.skillspace.method )
    return(res)
}
