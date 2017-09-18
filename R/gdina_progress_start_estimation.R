## File Name: gdina_progress_start_estimation.R
## File Version: 0.01
## File Last Change: 2017-06-05 13:53:09

gdina_progress_start_estimation <- function( progress, linkfct, disp, G, groupre, s1)
{
	if (progress){
		cat(disp,"\n")
		cat( " Link function:" , linkfct , "\n")
		if (G>1){ 
			cat(" Multiple group estimation with",G,"groups\n")
			if (groupre){ cat( "  Renumbered group identifier from 1 to",G,"\n") }
		}
		cat( "  **", paste(s1), "\n"   )
		cat("---------------------------------------------------------------------------------\n")
		utils::flush.console()
	}
}
