## File Name: slca_est_xlambda_constraint.R
## File Version: 0.04

slca_est_xlambda_constraint <- function( Xlambda, Xlambda.constr.V, V1, e2 )
{
    V <- Xlambda.constr.V
    if (! is.null(V)){
        e1 <- matrix( Xlambda, ncol=1 )
        Xlambda <- ( e1 + V %*% V1 %*% ( e2 - t(V) %*% e1 ) )[,1]
    }
    return(Xlambda)
}
