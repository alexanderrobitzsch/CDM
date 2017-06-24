
gdina_calc_expected_counts <- function( G , J , L , item.patt.freq, p.aj.xi, some.missings ,
		ipr , attr.patt.c, resp.patt, item.patt.split, data )
{        
	R.lj.gg <- I.lj.gg <- NULL	
	#---------------------- single groups ------------------------------------------    
	if (G == 1){ 	
		R.lj <- I.lj <- matrix( 0 , nrow=J , ncol=L )
		if ( some.missings ){
			I.lj <- crossprod( item.patt.freq*resp.patt	, p.aj.xi )
		} else {
			I.lj <- matrix( crossprod( item.patt.freq , p.aj.xi ) , nrow=J , ncol=L , byrow=TRUE )
		}												
		R.lj <- crossprod(ipr ,  p.aj.xi )
		colnames(I.lj) <- colnames(R.lj) <- attr.patt.c
		rownames(I.lj) <- rownames(R.lj) <- colnames(data)
	}	# end one group

	#---------------------- multiple groups ------------------------------------------			
	if (G > 1){ 					 
		R.lj.gg <- I.lj.gg <- array( 0 , c( J, L , G ) )
		for (gg in 1:G){ 	
			if ( some.missings ){
				I.lj.gg[,,gg] <- crossprod( item.patt.freq[,gg]*resp.patt , p.aj.xi[,,gg] )
			} else {
				I.lj.gg[,,gg] <- crossprod( item.patt.freq[,gg]*resp.patt , p.aj.xi[,,gg] )
			}
		    R.lj.gg[,,gg] <- crossprod( item.patt.split  * item.patt.freq[,gg] * resp.patt , p.aj.xi[,,gg] )
			colnames(I.lj.gg) <- colnames(R.lj.gg) <- attr.patt.c
			rownames(I.lj.gg) <- rownames(R.lj.gg) <- colnames(data)
		}
		# calculate I.lj and R.lj
		I.lj <- I.lj.gg[,,1]
		R.lj <- R.lj.gg[,,1]
		for (gg in 2:G){ 
			I.lj <- I.lj + I.lj.gg[,,gg]
			R.lj <- R.lj + R.lj.gg[,,gg]
		}
	}				
	#--- OUTPUT
	res <- list( I.lj=I.lj, R.lj=R.lj , I.lj.gg=I.lj.gg, R.lj.gg=R.lj.gg)
	return(res)
}