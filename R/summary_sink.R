## File Name: summary_sink.R
## File Version: 0.02
## File Last Change: 2017-01-31 14:07:30

summary_sink <- function( object , file , append=FALSE , ...){
	osink( file = file , suffix = "__SUMMARY.Rout" , append = append )
	print( summary(object=object , ... ) )
	csink(file=file)
}
