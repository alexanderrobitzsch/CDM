## File Name: gdina_create_attribute_patterns.R
## File Version: 0.05

gdina_create_attribute_patterns <- function( q.matrix, skillclasses, zeroprob.skillclasses,
        Z.skillspace, G, reduced.skillspace )
{

    # extract unique Q-matrix entries
    K <- ncol(q.matrix)
    q.entries <- as.list( 1:K )
    maxAttr <- rep(1,K)
    for (kk in 1:K){
        q.entries[[kk]] <- sort(unique( c(0,q.matrix[,kk] )))
        maxAttr[kk] <- length( q.entries[[kk]] ) - 1
    }

    attr.patt <- as.matrix( expand.grid( q.entries ) )
    if ( ! is.null(skillclasses) ){
        attr.patt <- skillclasses
    }
    colnames(attr.patt) <- colnames(q.matrix)
    L <- nrow(attr.patt)

    # combine all attributes in an attribute pattern as a string
    attr.patt.c <- apply( attr.patt, 1, FUN = function(ll){ paste(ll,collapse="" ) } )

    # create designmatrix for reduced skill space

    if ( is.null(reduced.skillspace) ){
        if (K < 4){
            reduced.skillspace <- FALSE
        } else {
            reduced.skillspace <- TRUE
        }
    }

    # if ( K < 4 | ( ! is.null( zeroprob.skillclasses ) ) | G > 1 ){
    if ( ! is.null( zeroprob.skillclasses )  ){
        reduced.skillspace <- FALSE
    }
    if ( ! is.null(Z.skillspace) ){
        reduced.skillspace <- TRUE
        Z.skillspace <- as.matrix(Z.skillspace)
    }
    Z <- NULL
    covbeta <- NULL
    beta <- NULL
    ncolZ <- nrow(attr.patt)-1
    #----- OUTPUT
    res <- list( K=K, maxAttr=maxAttr, attr.patt=attr.patt, L=L, attr.patt.c=attr.patt.c,
                    reduced.skillspace=reduced.skillspace, Z.skillspace=Z.skillspace, Z=Z, beta=beta,
                    covbeta=covbeta, ncolZ=ncolZ, q.entries=q.entries)
    return(res)
}
