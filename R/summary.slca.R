## File Name: summary.slca.R
## File Version: 1.29

#*******************************************************
# Summary for slca object
summary.slca <- function( object , file = NULL , ... )
{
	
	osink( file = file , suffix = paste0( "__SUMMARY.Rout") )

	cat("-----------------------------------------------------------------------------\n")

	#-- print package
	cdm_print_summary_package(pack="CDM")
	cat("\n")
	
	#-- print call
	cdm_print_summary_call(object=object)
	
	#-- print computation time
	cdm_print_summary_computation_time(object=object)
	
	cat("Structured Latent Class Analysis - Function 'slca' \n")
	modeltype <- object$irtmodel

	cat( "   " , object$N , "Cases, " , object$I , "Items, " , object$G , "Group(s)", "," ,
				object$TP , "Skill classes\n")  
	cat("\n **** Check carefully the number of parameters and identifiability
			of the model.  ***\n")

	#-- group statistics			
	if (object$G > 1 ){						
		cat("\nGroup statistics\n")
		print( object$group.stat )	
	}
	
	cat("\n-----------------------------------------------------------------------------\n")
	cat( "Number of iterations =" , object$iter , "\n" )
	if ( ! object$converged ){ cat("Maximum number of iterations was reached.\n") }
	cat( "Iteration with minimal deviance =" , object$iter.min , "\n" )
		
	cat( "\nDeviance = " , round( object$deviance , 2 ) , " | " )
	cat( "Log Likelihood = " , round( -object$deviance/2 , 2 ) , "\n" )	
	cat( "Penalty = " , round( object$regular_penalty  , 2 ) , "\n" )		
	
	cat( "Number of persons = " , object$ic$n , "\n" )    

	cat( "Number of estimated parameters = " , object$ic$np , "\n" )    
	cat( "  Number of estimated lambda parameters = " , object$ic$itempars , "\n" ) 
	cat( "  Number of non-active lambda parameters = " , object$ic$nonactive , "\n" ) 	
	cat( "  Number of estimated distribution parameters = " , object$ic$traitpars , "\n\n" )    

	cat( "Regularization = " , object$regularization , "\n" ) 	
	cat( "  Regularization method = " , object$regular_type , "\n" ) 	
	cat( "  Regularization parameter lambda = " , object$regular_lam , "\n\n" ) 	
	
	
	#-- information criteria
	cdm_print_summary_information_criteria(object=object)

	cat("-----------------------------------------------------------------------------\n")
	cat("Xlambda Parameters \n")
	obji <- object$Xlambda
	cdm_print_summary_data_frame(obji, digits=3)
	
	cat("-----------------------------------------------------------------------------\n")
	cat("Conditional Item Probabilities \n")
	obji <- object$item
	cdm_print_summary_data_frame(obji, from=3, digits=3)
	
	cat("-----------------------------------------------------------------------------\n")
	cat("Skill Class Parameters \n")
	obji <- object$delta
	cdm_print_summary_data_frame(obji, digits=3)
	
	cat("-----------------------------------------------------------------------------\n")
	cat("Skill Class Probabilities \n")
	obji <- object$pi.k
	cdm_print_summary_data_frame(obji, digits=4)

	csink( file = file )
}
#*******************************************************
