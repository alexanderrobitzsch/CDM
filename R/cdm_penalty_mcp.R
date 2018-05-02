## File Name: cdm_penalty_mcp.R
## File Version: 0.01

cdm_penalty_mcp <- function( x, lambda, a = 3.7 )
{
    y <- lambda * abs(x) - x^2 / ( 2 * a )
    y <- ifelse(  abs(x) > a*lambda ,  a * lambda^2 / 2 , y )
    return(y)
}
