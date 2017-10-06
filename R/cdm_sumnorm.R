## File Name: cdm_sumnorm.R
## File Version: 0.02
## File Last Change: 2017-10-05 18:05:47

cdm_sumnorm <- function(vec, norm=1)
{
	vec <- as.vector(vec)
	res <- vec / sum(vec) * norm
	return(res)
}
