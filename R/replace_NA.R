## File Name: replace_NA.R
## File Version: 0.01
## File Last Change: 2017-02-26 18:25:42

replace_NA <- function( matr , value = 0 )
{
	matr[ is.na(matr) ] <- value
	return(matr)
}
