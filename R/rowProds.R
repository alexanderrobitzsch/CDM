## File Name: rowProds.R
## File Version: 1.03
rowProds <- function(matr)
{

# Call: from din()
# Input: numeric matrix with positive entries
# Output: row products of input matrix

	exp( rowSums( log(matr + 10^(-300) ) ) )

}

