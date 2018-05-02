## File Name: gdd.R
## File Version: 0.14
#################################################################
# generalized distance discriminating method
gdd <- function( data , q.matrix , theta , b , a  , skillclasses=NULL)
{
    data <- as.matrix(data)
    data_isna <- is.na(data)
    dataresp <- as.matrix( 1 - data_isna )
    data[ data_isna ] <- 0
    q.matrix <- as.matrix(q.matrix)
    skillspace <- skillclasses
    # compute ideal response pattern
    res <- ideal.response.pattern( q.matrix , skillspace )
    idealresp <- res$idealresp
    skillspace <- res$skillspace
    # apply generalized distance discriminating method written in Rcpp
    res <- cdm_rcpp_generalized_distance_method( data=data,
            dataresp=dataresp, idealresp=idealresp, theta=theta, a=a, b=b )
    # extract results
    distmatrix <- res$dist
    skillclass.est <- skillspace[ res$est_skill , ]
    res <- list( skillclass.est = skillspace , distmatrix = distmatrix ,
                    skillspace = skillspace , theta = theta )
    return(res)
}
###############################################################################
