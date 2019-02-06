## File Name: din_identifiability.R
## File Version: 0.06

din_identifiability <- function(q.matrix)
{
    K <- ncol(q.matrix)
    I <- nrow(q.matrix)
    if (is.null(colnames(q.matrix))){
        colnames(q.matrix) <- paste0("Skill", 1:K)
    }
    skills <- colnames(q.matrix)

    #* search for identity matrix (single loadings)
    vec0 <- rep(0,K)
    index_single <- cdm_create_vector(names=skills, val=NA)
    is_single <- cdm_create_vector(names=skills, val=FALSE)

    for (kk in 1:K){
        vec1 <- vec0
        vec1[kk] <- 1
        rm1 <- rowMeans( q.matrix==cdm_matrix2(vec1, nrow=I) )
        ind1 <- which( rm1==1 )[1]
        index_single[kk] <- ind1
        if ( ! is.na(ind1) ){
            is_single[kk] <- TRUE
        }
    }

    #- each of the attributes measured by at least three items
    skills_items <- colSums(q.matrix)
    is_three_items <- skills_items >=3

    #- distinctness of columns in submatrix
    submat_distinct <- TRUE
    Q_ast <- q.matrix[-na.omit(index_single),]
    for (ii in 1:(K-1)){
        for (jj in (ii+1):K){
            submat_distinct <- submat_distinct & ( ! ( mean( Q_ast[,ii]==Q_ast[,jj] )==1 ) )
        }
    }

    # Q-matrix information
    item_M <- mean( rowSums(q.matrix) )
    qmat_stat <- list(item_M=item_M, skills_items=skills_items)

    dina_identified <- prod(is_single) & prod(is_three_items) & submat_distinct

    #-- output
    res <- list( index_single=index_single, is_single=is_single,
                    is_three_items=is_three_items, submat_distinct=submat_distinct,
                    q.matrix=q.matrix, I=I, K=K, qmat_stat=qmat_stat,
                    dina_identified=dina_identified)
    class(res) <- "din_identifiability"
    return(res)
}
