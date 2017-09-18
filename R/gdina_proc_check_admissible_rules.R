## File Name: gdina_proc_check_admissible_rules.R
## File Version: 0.01
## File Last Change: 2017-06-05 13:29:26

gdina_proc_check_admissible_rules <- function(rule)
{
	admiss.rules <- c("GDINA" , "ACDM" , "DINA" , "DINO" ,
							"GDINA1" , "GDINA2" , "RRUM" )
	i1 <- which( ! ( rule %in% admiss.rules ) )
	if ( length(i1) > 0 ){
		cat("The following rules are not implemented in gdina: ")
		cat( paste( unique( rule[i1] ) , collapse= " " ) , "\n" )
		stop("Change your argument 'rule'")
	}
}
