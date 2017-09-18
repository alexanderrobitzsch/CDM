## File Name: rowProds.R
## File Version: 1.02
## File Last Change: 2017-01-31 14:07:29
rowProds <-
function(matr){

# Call: from din()
# Input: numeric matrix with positive entries
# Output: row products of input matrix

    exp( rowSums( log(matr + 10^(-300) ) ) )
    
}

