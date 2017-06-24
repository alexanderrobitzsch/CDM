
replace_NA <- function( matr , value = 0 )
{
	matr[ is.na(matr) ] <- value
	return(matr)
}