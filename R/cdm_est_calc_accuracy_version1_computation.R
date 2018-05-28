## File Name: cdm_est_calc_accuracy_version1_computation.R
## File Version: 0.01




#####################################################################################
# estimate classification accuracy and consistency
cdm_est_calc_accuracy_version1_computation <- function( p.xi.aj, est.class, class.prob )
{
    m0 <- matrix(  colSums( p.xi.aj ), nrow=nrow(p.xi.aj), ncol=ncol(p.xi.aj), byrow=TRUE )
    p.xi.aj <- p.xi.aj  / m0
    # calculate class index
    est.class.index <- match( paste(est.class), colnames(p.xi.aj ) )
    # calculate formula (5)
    CC <- ncol( p.xi.aj )
    # classification probability matrix
    class.prob.matrix2 <- class.prob.matrix1 <- matrix( NA, CC, CC )
    for (aa in 1:CC){
        for (cc in 1:CC){
            class.prob.matrix1[cc,aa] <- sum( p.xi.aj[ est.class.index==cc, aa ] )^2
            class.prob.matrix2[cc,aa] <- sum( p.xi.aj[ est.class.index==cc, aa ] )
        }
    }
    # classification consistency
    P_c <- sum( colSums( class.prob.matrix1 ) * class.prob )
    # marginal classification accuracy
    P_a <- sum( diag( class.prob.matrix2 ) * class.prob )
    #**** calculate kappa
    M1 <- class.prob.matrix1
    p1 <- rowSums(M1)
    p2 <- colSums(M1)
    h1 <- outer( p1, p2 )
    #--- output
    res <- data.frame( P_c=P_c, P_a=P_a )
    return(res)
}
#####################################################################################

.est.class.accuracy <- cdm_est_calc_accuracy_version1_computation
