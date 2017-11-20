## File Name: IRT_se_gdina.R
## File Version: 0.14

IRT_se_gdina <- function(object, h=1E-4)
{


	dat <- object$dat
	resp.ind.list <- object$resp.ind.list
	delta <- object$delta
	attr.prob <- object$attr.prob
	aggr.attr.patt <- object$control$aggr.attr.patt
	Aj <- object$Aj
	Mj <- object$Mj
	p.xi.aj <- object$like
	J <- ncol(dat)
	linkfct <- object$control$linkfct
	item.patt.split <- object$control$item.patt.split
	item.patt.freq  <- object$item.patt.freq
	IP <- nrow(item.patt.split)
	zeroprob.skillclasses <- object$zeroprob.skillclasses
	resp.ind.list <- object$resp.ind.list
	G <- object$G	
	L <- object$Nskillclasses
	group <- object$G
	reduced.skillspace <- object$reduced.skillspace
	beta <- object$beta
	Z.skillspace <- object$Z.skillspace
	
	if (G==1){
		item.patt.freq <- as.vector(item.patt.freq)
	}
	
	progress <- FALSE
	iter <- 1E2
		
	#-- handle reduced skill space
	if (reduced.skillspace){
		if (G==1){	
			attr.prob <- reduced_skillspace_beta_2_probs( Z=Z.skillspace, beta=beta )
		}
		if (G>1){
			attr.prob <- matrix(NA, nrow=L, ncol=G)
			for (gg in 1:G){
				attr.prob[,gg] <- reduced_skillspace_beta_2_probs( Z=Z.skillspace, beta=beta[,gg] )			
			}
		}
	}
	# no reduced skillspace
	if (! reduced.skillspace){
		eps <- 2*h
		beta <- matrix( 0 , nrow=L-1, ncol=G)		
		bounds <- c(eps, 1E3)
		if (G==1){
			beta[,1] <- cdm_sumnorm_squeeze( vec=attr.prob, bounds=bounds )[-L]
		} else {
			for (gg in 1:G){
				beta[,gg] <- cdm_sumnorm_squeeze( vec=attr.prob[,gg], bounds=bounds )[-L]
			}
		}
	}
	
    arglist <- list( J=J, L=L, aggr.attr.patt=aggr.attr.patt, Mj=Mj, 
					delta=delta, linkfct=linkfct, IP=IP, 
					item.patt.split=item.patt.split, resp.ind.list=resp.ind.list, 
					zeroprob.skillclasses=zeroprob.skillclasses, G=G, item.patt.freq=item.patt.freq,
					beta=beta, Z.skillspace=Z.skillspace, reduced.skillspace=reduced.skillspace, 
					return_all=TRUE) 
	res <- do.call( what=IRT_se_gdina_calc_individual_likelihood, args=arglist )
	loglike <- res$loglike
	p.xi.aj <- res$p.xi.aj
	p.aj.xi <- res$p.aj.xi
	like_ind <- res$like_ind
	
	#--- derivatives based with respect to delta	
	
	delta1 <- delta
	
	jj <- 5  # item 
	pp <- 1  # parameter
	delta1[[jj]][[pp]] <- delta[[jj]][[pp]] + h
	arglist1 <- arglist
	arglist1$return_all <- FALSE	
	arglist1$delta <- delta1
	loglike1 <- do.call( what=IRT_se_gdina_calc_individual_likelihood, args=arglist1 )	

	d1 <- ( loglike1 - loglike ) / h

# Revalpr("d1")	
	
	eps <- 1E-20
	
	prob_args <- list( J=J , jj=jj, L=L, aggr.attr.patt=aggr.attr.patt, Mj=Mj, delta=delta, linkfct=linkfct)				
	pj0 <- do.call( gdina_calc_prob_one_item , args = prob_args )
	
	prob_args1 <- prob_args
	delta1 <- delta
	delta1[[jj]][[pp]] <- delta[[jj]][[pp]] + h
	prob_args1$delta <- delta1
	pj1 <- do.call( gdina_calc_prob_one_item , args = prob_args1 )

	delta1 <- delta
	delta1[[jj]][[pp]] <- delta[[jj]][[pp]] - h
	prob_args1$delta <- delta1
	pj2 <- do.call( gdina_calc_prob_one_item , args = prob_args1 )	
		
	like_der <- matrix( 0, nrow=IP, ncol=L)
	resp_jj <- resp.ind.list[[jj]]
	data_jj <- item.patt.split[ resp_jj , jj ] + 1
	like_der[ resp_jj , ] <- ( pj1[ data_jj , ] - pj2[ data_jj , ] ) / (2*h) / pj0[ data_jj , ]
	
	if (G==1){
		d1a <- sum( rowSums( p.aj.xi * like_der ) * item.patt.freq )
	} else {
		d1a <- 0
		for (gg in 1:G){
			d1a <- d1a + sum( rowSums( p.aj.xi[,,gg] * like_der ) * item.patt.freq[,gg] )
		}
	}
# Revalpr("d1a")	

	#---- derivatives with respect to beta

	bb <- 4
	gg <- 1
	
	beta1 <- beta
	beta1[bb,gg] <- beta[bb,gg] + h
	arglist1 <- arglist
	arglist1$return_all <- FALSE	
	arglist1$beta <- beta1
	loglike1 <- do.call( what=IRT_se_gdina_calc_individual_likelihood, args=arglist1 )	
	d1 <- ( loglike1 - loglike ) / h
	
	beta1 <- beta
	beta1[bb,gg] <- beta[bb,gg] - h
	arglist1 <- arglist
	arglist1$return_all <- FALSE	
	arglist1$beta <- beta1
	loglike2 <- do.call( what=IRT_se_gdina_calc_individual_likelihood, args=arglist1 )	
	d1b <- ( loglike1 - loglike2 ) / (2*h)

# Revalpr("d1b")
		
	beta1[bb,gg] <- beta[bb,gg] + h	
    skill_args <- list( beta=beta1, Z.skillspace=Z.skillspace, reduced.skillspace=reduced.skillspace, G=G) 
    attr.prob1 <- do.call( what=IRT_se_gdina_calc_skill_distribution, args=skill_args )
						
	beta1[bb,gg] <- beta[bb,gg] - h
	skill_args$beta <- beta1
	attr.prob2 <- do.call( what=IRT_se_gdina_calc_skill_distribution, args=skill_args )

	if (G==1){
		attr_prob1 <- attr.prob1
		attr_prob2 <- attr.prob2
		attr_prob <- attr.prob
		post <- p.aj.xi
		freq <- item.patt.freq
	} else {
		attr_prob1 <- attr.prob1[,gg]
		attr_prob2 <- attr.prob2[,gg]
		attr_prob <- attr.prob[,gg]
		post <- p.aj.xi[,,gg]
		freq <- item.patt.freq[,gg]
	}	
	M1 <- cdm_matrix2( ( attr_prob1 - attr_prob2 ) / (2*h) / ( attr_prob + eps ) , nrow=IP)	
	d1a <- sum( rowSums( post * M1 ) * freq )

	
# Revalpr("d1a")		
		
	### This function is yet incomplete. But the remaining derivatives can be simply calculated.

}
