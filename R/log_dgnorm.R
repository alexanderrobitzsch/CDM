## File Name: log_dgnorm.R
## File Version: 0.06

## kernel of the generalized normal distribution
log_dgnorm <- function( x, loc, scale, power )
{
    if ( power < 1E3 ){
        y <- - ( abs( x - loc ) / scale )^power / power
    } else {
        y <- rep(0, length(x) )
    }
    return(y)
}
