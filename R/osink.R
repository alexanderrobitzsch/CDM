## File Name: osink.R
## File Version: 1.04
## File Last Change: 2017-01-31 14:07:29

# 	osink( file = file , suffix = "__SUMMARY.Rout" )

#   csink( file = file )


osink <- function( file , suffix, append = FALSE){
	if ( ! is.null( file ) ){
		sink( paste0( file , suffix) , split=TRUE , append = append )
						}
				}
				
csink <- function( file){
	if ( ! is.null( file ) ){  
	   sink()	
				}	
					}
