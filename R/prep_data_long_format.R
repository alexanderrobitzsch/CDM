## File Name: prep_data_long_format.R
## File Version: 0.03

prep_data_long_format <- function(data)
{
    data_long <- cdm_rcpp_data_prep_long_format(data=data)
    class(data_long) <- c("matrix", "data_long_format")
    return(data_long)
}
