## File Name: rowProds2.R
## File Version: 1.02

#************************************************************************
rowProds2 <- function(matr)
{
    y <- matr[,1]
    for (ii in 2:dim(matr)[2]){
        y <- y * matr[,ii]
    }
    return(y)
}
#...................................................................
