## File Name: cdm_matrixstring.R
## File Version: 0.01


############################################################
# calculates a string pattern consisting of matrix entries
# matr <- skillclasses
# string <- "Q"
cdm_matrixstring <- function( matr, string )
{
    VV <- ncol(matr)
    l1 <- string
    for ( vv in 1:VV){
        l1 <- paste0( l1, matr[,vv] )
    }
    return(l1)
}
#################################################################
