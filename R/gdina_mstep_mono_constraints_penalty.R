## File Name: gdina_mstep_mono_constraints_penalty.R
## File Version: 0.04

gdina_mstep_mono_constraints_penalty <- function(x)
{
    y <- ifelse( x < 0, x^2, 0 )
    return(y)
}
