
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
