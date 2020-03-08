## File Name: ideal.response.pattern.R
## File Version: 0.137


#-- computation of ideal response pattern
ideal.response.pattern <- function( q.matrix, skillspace=NULL, rule="DINA" )
{
    K <- ncol(q.matrix)
    q.matrix0 <- q.matrix
    if ( is.null(skillspace) ){
        skillspace <- data.frame( rbind( rep(0,K), rep(1,K) ) )
        skillspace <- as.matrix( expand.grid( as.list( skillspace ) ) )
        if ( ! is.null( colnames(q.matrix) ) ){
            colnames(skillspace) <- colnames(q.matrix)
        }
    }
    skillspace0 <- skillspace
    if (rule=="DINO"){
        skillspace <- 1-skillspace
    }
    # compute ideal response pattern
    skillspace <- as.matrix(skillspace)
    q.matrix <- as.matrix(q.matrix)
    idealresp <- cdm_rcpp_ideal_resp_pattern( qmatrix=q.matrix, skillspace=skillspace )
    #* DINO rule
    if (rule=="DINO"){
        idealresp <- 1-idealresp
    }
    #-- output
    res <- list( idealresp=idealresp, skillspace=skillspace0, rule=rule,
                    q.matrix=q.matrix0 )
    return(res)
}
