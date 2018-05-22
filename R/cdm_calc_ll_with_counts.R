## File Name: cdm_calc_ll_with_counts.R
## File Version: 0.03

cdm_calc_ll_with_counts <- function( an.ik, pjk, eps=1E-20 )
{
    colSums( colSums( an.ik * log(pjk+eps) ))
}
