## File Name: slca_print_progress_em_algorithm.R
## File Version: 0.01
## File Last Change: 2017-10-04 16:51:42

slca_print_progress_em_algorithm <- function(progress, disp, iter, dev, dev0, deltadiff, Xlambda_change)
{
	if (progress){
		cat(disp)	
		cat("Iteration" , iter , "   " , paste( Sys.time() ) , "\n" )		
		cat( paste( "   Deviance = "  , 
					round( dev , 4 ) , 
					if (iter > 1 ){ " | Deviance change = " } else {""} ,
							 if( iter>1){round( - dev + dev0 , 6 )} else { ""}	,sep=""))
		if ( dev > dev0 & (iter>1 ) ){ cat( "  Deviance increases!") } ; cat("\n")
		cat( paste( "    Maximum Xlambda parameter change = " , 
								 round( max( Xlambda_change ) , 6 ) ,  " \n"   )  )  
		cat( paste( "    Maximum distribution parameter change = " , 
								 round( max( deltadiff ) , 6 ) ,  " \n"   )  )  
		utils::flush.console()
	}
}
