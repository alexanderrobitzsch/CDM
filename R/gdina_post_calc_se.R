## File Name: gdina_post_calc_se.R
## File Version: 0.02

gdina_post_calc_se <- function(G, p.aj.xi, item.patt.freq, attr.prob, p.xi.aj, IP, J,
		calc.se, aggr.attr.patt, Aj, Mj, R.lj, I.lj, item.patt.split, resp.patt, delta, linkfct,
		rule, avoid.zeroprobs, data, se_version, method, delta.fixed, q.matrix )
{
	varmat.delta <- NULL
	varmat.palj <-  NULL
	se.delta <- NULL	
	delta.summary <- NULL
	if (G == 1){ 	
		PAJXI <-  p.aj.xi
	}
	if (G>1){			
		a1 <- outer( rep(1,nrow(attr.prob) ) , colSums( item.patt.freq ) ) / sum( item.patt.freq)
		attr.prob.tot <- rowSums( attr.prob * a1 )
		PAJXI <- outer( rep(1,IP), attr.prob.tot ) * p.xi.aj
		PAJXI <- PAJXI / rowSums(PAJXI)
	}

	# matrix form of item.patt.freq
	if (G==1){ 
		item.patt.freq <- matrix( item.patt.freq , ncol=1 ) 
	}
	freq.pattern <- rowSums( item.patt.freq )
	
	eps2 <- 1E-10	
	for (jj in 1:J){	
		se.jj <- NA
		if ( calc.se ){
			Ajjj <- Aj[[jj]]
			Mjjj <- Mj[[jj]][[1]]
			apjj <- aggr.attr.patt[[jj]]
			R.lj_jj <- R.lj[jj,]
			I.lj_jj <- I.lj[jj,]
			Mjj2 <- Mj[[jj]][[2]]
			item.patt.split_jj <- item.patt.split[,jj]
			resp.patt_jj <- resp.patt[,jj]
			delta_jj <- delta[[jj]]
			res_jj <- gdina_se_itemwise( R.lj_jj=R.lj_jj, I.lj_jj=I.lj_jj, apjj=apjj, Mjjj=Mjjj, Mjj2=Mjj2, 
							PAJXI=PAJXI, IP=IP, item.patt.split_jj=item.patt.split_jj, resp.patt_jj=resp.patt_jj, 
							freq.pattern=freq.pattern, item.patt.freq=item.patt.freq, 
							avoid.zeroprobs =avoid.zeroprobs, data=data, jj=jj, method=method, 
							linkfct=linkfct, delta_jj=delta_jj, 
							se_version=se_version )	
			varmat.delta[[jj]] <- res_jj$varmat.delta_jj
			varmat.palj[[jj]] <- res_jj$varmat.palj_jj
			se.jj <- sqrt( diag(varmat.delta[[jj]] )  ) 
		}

		delta.summary.jj <-	data.frame( link=linkfct, item=colnames(data)[jj], itemno=jj, 
							type=Mj[[jj]][2], rule=rule[jj], est=delta[[jj]], se=se.jj )
		# fix delta parameter here!!
		if ( ! is.null( delta.fixed ) ){
			delta.fixed.jj <- delta.fixed[[jj]]
			if ( ! is.na( delta.fixed.jj)[1] ){
					delta.summary.jj$se <- 0
			}
		}								
								
		colnames(delta.summary.jj)[4] <- "partype"					
		delta.summary <- rbind( delta.summary , delta.summary.jj )								
	}				
	
	delta.summary$partype.attr <- paste(delta.summary$partype)
	if (calc.se){		
		for (jj in 1:J){
			ind.jj <- which( delta.summary$itemno == jj )
			qjj <- which( q.matrix[ jj , ]	> 0 )
			pgjj <- pajj <- paste(delta.summary$partype.attr[ind.jj])
			cjj <- paste(colnames(q.matrix)[qjj])
			NN <- length(pajj)
			pajj <- gsub( "|" , "-" , pajj )
			pajj <- gsub( "=" , "-" , pajj )
			for (nn in 1:NN){
				st1 <- as.numeric(unlist( strsplit( paste(pajj[nn]) , "-" ) ))
				st1 <- st1[ ! is.na( st1 ) ]
				st1 <- st1[ st1 > 0 ]
				pgjj[nn] <- paste( cjj[ st1 ] , collapse="-" )
			}
			delta.summary$partype.attr[ind.jj] <- pgjj
		}
	}
	#--- OUTPUT	
	res <- list( varmat.delta=varmat.delta, varmat.palj=varmat.palj, se.delta=se.delta,
					delta.summary=delta.summary, freq.pattern=freq.pattern,
					item.patt.freq=item.patt.freq)
	return(res)
}
	
