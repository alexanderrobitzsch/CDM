
summary.IRT.RMSD_print_statistics <- function( stat_summary, stat, digits){
	obji <- stat_summary
	NC <- ncol(obji)
	for (gg in 2:NC ){
		obji[,gg] <- round( obji[,gg] , digits=digits)
	}
	print( obji )   	
	cat("\n")
	obji <- stat
	NC <- ncol(obji)
	for (gg in 2:NC ){
		obji[,gg] <- round( obji[,gg] , digits=digits)
	}
	print( obji )   
}
