## File Name: gdina_proc_prior_distribution_extend_normal.R
## File Version: 0.01


gdina_proc_prior_distribution_extend_normal <- function(prior)
{
    if (length(prior)==2){
        prior <- c(prior, 2)
    }
    return(prior)
}
