## File Name: din.equivalent.class.R
## File Version: 0.191



#**** calculation of equivalent skill classes
din.equivalent.class <-function( q.matrix, rule="DINA")
{
    Q <- q.matrix
    # Matrix with all skill classes
    S <- expand.grid( as.data.frame( t( matrix( rep( c(0,1), each=ncol(Q)), ncol=2 ))))
    J <- nrow(Q)
    if ( length(rule)==1){ rule <- rep( rule, J ) }
    rownames(S) <- paste0("Skills_", apply( S, 1,
            FUN=function(ll){ paste(ll, collapse="" ) } )  )

    # Calculation of latent response of every skill class
    A <- din_equivalent_class_latent_response(q.matrix=Q,S=S,rule=rule)
    A <- t(A)
    I <- nrow(A)
    # calculate latent responses
    latent.response <- paste0("LatResp_",
        sapply( 1:I, FUN=function(ii){ paste( A[ ii, ], collapse="" )  } ) )

    skillclasses <- data.frame( "skillclass"=rownames(S) )
    skillclasses$latent.response <- latent.response

    # define distinguishable skill classes
    skillclasses$distinguish.class <- match( latent.response, unique( latent.response ) )
    # calculates how many skill classes correspond to the same latent response
    latent.response <- table( latent.response )
    six <- sort( latent.response, index.return=FALSE, decreasing=TRUE)
    gini_mod <- cdm_gini( as.numeric(six) )
    res <- list( "latent.responseM"=A, "latent.response"=latent.response,
                "S"=S, "gini"=gini_mod, "skillclasses"=skillclasses )
    cat( nrow(S), "Skill classes |", max( skillclasses$distinguish.class ),
        " distinguishable skill classes |",
        "Gini coefficient=", round( gini_mod,3 ), "\n")
    return(res)
}

