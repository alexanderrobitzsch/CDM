## File Name: IRT_se_gdina_calc_skill_distribution.R
## File Version: 0.06

IRT_se_gdina_calc_skill_distribution <- function(beta, Z.skillspace, reduced.skillspace, G, eps = 1E-5 )
{
#--- adapt to multiple group case

    #--- reduced skill space
    if (reduced.skillspace){
        L <- nrow(Z.skillspace)
        if (G==1){
            attr.prob <- reduced_skillspace_beta_2_probs( Z=Z.skillspace, beta=beta )
        } else {
            attr.prob <- matrix( NA, nrow=L, ncol=G)
            for (gg in 1:G){
                attr.prob[,gg] <- reduced_skillspace_beta_2_probs( Z=Z.skillspace, beta=beta[,gg] )
            }
        }
    }
    #--- no reduced skill space
    if ( ! reduced.skillspace ){
        bounds <- c(eps, 1E2)
        if (G==1){
            attr.prob <- c( beta , 1 - sum(beta) )
            attr.prob <- cdm_sumnorm_squeeze(vec=attr.prob, bounds=bounds)
        } else {
            L <- nrow(beta) + 1
            attr.prob <- matrix( NA, nrow=L, ncol=G)
            for (gg in 1:G){
                b1 <- beta[,gg]
                b1 <- c( b1, 1 - sum(b1) )
                attr.prob[,gg] <- cdm_sumnorm_squeeze(vec=b1, bounds=bounds)
            }
        }
    }
    return(attr.prob)
}
