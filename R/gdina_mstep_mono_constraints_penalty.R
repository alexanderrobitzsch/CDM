## File Name: gdina_mstep_mono_constraints_penalty.R
## File Version: 0.06

gdina_mstep_mono_constraints_penalty <- function(x)
{
    eps <- 1e-3
    # y <- ifelse( x < 0, x^2, 0 )
    y <- ifelse( x < 0, sqrt(x^2+eps), 0 )
    return(y)
}
