## File Name: gdm_proc_response_indicators.R
## File Version: 0.02

gdm_proc_response_indicators <- function(dat.resp)
{
	I <- ncol(dat.resp)
	resp.ind.list <- list( 1:I )
	for (ii in 1:I){ 
		resp.ind.list[[ii]] <- which( dat.resp[,ii] == 1)  
	}	
	return(resp.ind.list)
}
