## File Name: array3_sum.R
## File Version: 0.01

#*** summation of the third margin in a three-dimensional array
array3_sum <- function( arr  )
{
	dimA <- dim(arr)
	K <- dimA[ 3 ]
	h1 <- arr[,,1]
	if (K>=2){
		for (kk in 2:K){
			h1 <- h1 + arr[,,kk]
		}
	}
	return(h1)
}
