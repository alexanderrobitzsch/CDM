
gdina_mstep_item_parameters <- function(R.lj, I.lj, aggr.patt.designmatrix, max.increment ,
		increment.factor, J, Aj, Mj, delta, method, avoid.zeroprobs, invM.list, linkfct,
		rule, iter, fac.oldxsi, rrum.model, delta.fixed, devchange, mstep_iter , mstep_conv,
		Mj.index , suffstat_probs )
{

	# calculation of expected counts
	R.ljM <- R.lj %*% aggr.patt.designmatrix
	I.ljM <- I.lj %*% aggr.patt.designmatrix

	eps2 <- eps <- 1E-10
    max.increment <- max.increment / increment.factor
	
	delta.new <- NULL
	for (jj in 1:J){ 	# begin item
		Ajjj <- Aj[[jj]]
		Mjjj <- Mj[[jj]][[1]]
		Rlj.ast <- R.ljM[ jj, Mj.index[jj,5]:Mj.index[jj,6] ]
		Ilj.ast <- I.ljM[ jj, Mj.index[jj,5]:Mj.index[jj,6] ]
		pjjj <- Rlj.ast / ( Ilj.ast + eps2 )
		suffstat_probs[[jj]] <- pjjj
		#--- define argument list
		if ( method %in% c("ULS","WLS","ML") ){		
			arglist <- list( pjjj=pjjj, Ilj.ast=Ilj.ast, Rlj.ast=Rlj.ast, eps=eps, 
							avoid.zeroprobs=avoid.zeroprobs, Mjjj=Mjjj, invM.list=invM.list, linkfct=linkfct, rule=rule, 
							method=method, iter=iter, delta.new=delta.new, max.increment=max.increment, fac.oldxsi=fac.oldxsi, 
							jj=jj, delta=delta, rrum.model=rrum.model, delta.fixed=delta.fixed, devchange=devchange ) 		
		}		
		#*** optimization ULS / WLS		
		if ( method %in% c("ULS","WLS") ){
			res_jj <- do.call("gdina_mstep_item_uls" , arglist )
		}
		#*** optimization ML
		rrum <- ( rule[jj] == "ACDM" )	& ( linkfct == "log")
		if ( method %in% c("ML") ){
			arglist$mstep_iter <- mstep_iter
			arglist$mstep_conv <- mstep_conv
			if ( ! rrum ){
				res_jj <- do.call("gdina_mstep_item_ml" , arglist )
			}
			if (  rrum ){
				res_jj <- do.call("gdina_mstep_item_ml_rrum" , arglist )			
			}												
		}						
		delta.new <- res_jj$delta.new					
	}		# end item
	#----------------- OUTPUT -------------
	res <- list( delta.new = delta.new, suffstat_probs=suffstat_probs )
	return(res)
}
