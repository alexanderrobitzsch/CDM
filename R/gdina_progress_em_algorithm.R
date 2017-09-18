## File Name: gdina_progress_em_algorithm.R
## File Version: 0.02
## File Last Change: 2017-06-05 12:25:05

gdina_progress_em_algorithm <- function( delta , data , like.new , loglikeold ,
		max.par.change , iter , progress, progress.item )
{
    if (progress) {  
		if (progress.item){ 
			g1 <- unlist( lapply( delta , FUN = function(ll){ paste( round(ll,4) , collapse= " " ) } ))
			g1 <- matrix( paste( colnames(data) , g1 ) , ncol=1)
			print(g1)
		}
		cat( "Deviance = "  , round( -2*like.new , 5 ) )
        devchange <- 2*(like.new-loglikeold)		
		if (iter >1){ 
			cat(" | Deviance change = " , round( 2*(like.new-loglikeold), 7) ) 
		}
		cat("\n" )
		if ( devchange < 0 & iter>1){ 
			cat( "**** Deviances decreases! Check for nonconvergence.   ****\n") 
		}
		cat("Maximum parameter change:" , round( max.par.change, 6), "\n") 			
	}
}
