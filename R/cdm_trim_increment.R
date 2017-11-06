## File Name: cdm_trim_increment.R
## File Version: 0.03

cdm_trim_increment <- function( increment, max.increment, type=1 )
{
	increment[ is.na(increment) ] <- 0
	if ( type == 1){
		increment <- ifelse(abs(increment)> max.increment, 
					sign(increment)*max.increment , increment )	
	}
	if ( type == 2){
		eps <- 1E-80
		ci <- ceiling( abs(increment) / ( abs( max.increment) + eps ) )
		increment <- ifelse( abs( increment) > abs(max.increment)  , 
                                 increment/(2*ci) , increment )	
	}		
	return(increment)
}
