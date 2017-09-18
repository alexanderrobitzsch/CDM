## File Name: IRT_RMSD_proc_dist_item.R
## File Version: 0.02
## File Last Change: 2017-02-26 19:06:04

IRT_RMSD_proc_dist_item <- function(dist.item)
{
	#--- processing
	dist.item <- replace_NA( dist.item , value = 0 )
	h1 <- array3_sum( dist.item )
	return(h1)
}
