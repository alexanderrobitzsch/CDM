## File Name: replace_NA.R
## File Version: 0.01

replace_NA <- function( matr , value = 0 )
{
    matr[ is.na(matr) ] <- value
    return(matr)
}
