## File Name: gdina_maximum_parameter_change.R
## File Version: 0.01
## File Last Change: 2017-06-04 19:30:03

gdina_maximum_parameter_change <- function( delta, delta.new, linkfct )
{
	max.par.change <- max( abs( unlist( delta.new ) - unlist( delta ) ) )
	if ( linkfct %in% c("logit","log") ){
		max.par.change <- max( abs( stats::plogis(unlist( delta.new )) - stats::plogis( unlist( delta ) )) )
	}
	return(max.par.change)
}
