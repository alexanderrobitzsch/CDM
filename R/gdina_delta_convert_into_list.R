## File Name: gdina_delta_convert_into_list.R
## File Version: 0.01


gdina_delta_convert_into_list <- function( delta_vec, delta_indices, J)
{
    delta <- as.list(1:J)
    for ( ii in 1:J){
        delta[[ ii ]] <- delta_vec[ delta_indices[[ii]] ]
    }
    return(delta)
}
