## File Name: IRT_frequencies_default.R
## File Version: 0.05
## File Last Change: 2017-06-24 15:26:59

IRT_frequencies_default <- function(data, post, probs, weights=NULL)
{
	dim_probs <- dim(probs)
	K <- dim_probs[2]
	I <- dim_probs[1] 
	TP <- dim_probs[3]
	N <- nrow(data)
	#--- preparation item response data
	dat <- data
	dat.ind <- 1 - is.na(dat)
	dat[ is.na(dat) ] <- 0
	if (is.null(weights)){
		weights <- rep(1,N)
	}
	#--- univariate observed frequencies
	uni_obs <- matrix( 0 , nrow=I , ncol=K)
	items <- colnames(dat)
	categories <- paste0("Cat" , 1:K - 1 )
	rownames(uni_obs) <- items
	colnames(uni_obs) <- categories
	for (kk in 1:K){
		uni_obs[,kk] <- colSums( ( dat == kk - 1) * dat.ind * weights )
	}
	#--- univariate expected frequencies
	uni_exp <- 0*uni_obs
	for (ii in 1:I){
		for (kk in 1:K){
			counts_ii_kk <- dat.ind[,ii] * weights * post
			uni_exp[ii,kk] <- sum( t(counts_ii_kk) * probs[ii,kk,] )
		}
	}
	#--- univariate marginals descriptive statistics
	M_obs <- rep(NA, I)
	names(M_obs) <- items
	SD_obs <- SD_exp <- M_exp <- M_obs
	for (ii in 1:I){
		res <- univar_table_statistics( freq = uni_obs[ii,] )
		M_obs[ii] <- res$M
		SD_obs[ii] <- res$SD
		res <- univar_table_statistics( freq = uni_exp[ii,] )
		M_exp[ii] <- res$M
		SD_exp[ii] <- res$SD
	}

	#--- bivariate frequency tables
	biv_obs <- array( 0 , dim = c(I,I,K,K) )
	dimnames(biv_obs) <- list( items, items, categories , categories )
	biv_exp <- biv_obs
	biv_N <- matrix(0,nrow=I,ncol=I)
	dimnames(biv_N) <- list(items, items)
	chisq <- cov_obs <- cov_exp <- cor_exp <- cor_obs <- biv_N
	attr( chisq , "df") <- chisq
	for (ii in 1:I){
		for (jj in ii:I){
			for (kk in 1:K){
				for (hh in 1:K){
					dat_temp <- ( dat[,ii] == kk-1 ) * ( dat[,jj] == hh - 1 ) * dat.ind[,ii] * dat.ind[,jj] * weights 
					biv_obs[ii,jj,kk,hh] <- sum( dat_temp )
					counts_temp <- dat.ind[,ii] * dat.ind[,jj] * weights * post
					biv_exp[ii,jj,kk,hh] <- sum( t(counts_temp) * probs[ii,kk,] * probs[jj,hh,] )
				}
			}
			biv_N[ii,jj] <- biv_N[jj,ii] <- sum(biv_obs[ii,jj,,])
			biv_obs[jj,ii,,] <- biv_obs[ii,jj,,]
			biv_exp[jj,ii,,] <- biv_exp[ii,jj,,]
		}
	}	
	for (ii in 1:I){
		for (jj in ii:I){
			res <- bivariate_table_statistics(freqtable=biv_obs[ii,jj,,])
			cov_obs[ii,jj] <- cov_obs[jj,ii] <- res$cov
			cor_obs[ii,jj] <- cor_obs[jj,ii] <- res$cor
			res <- bivariate_table_statistics(freqtable=biv_exp[ii,jj,,])
			K1 <- res$K1
			K2 <- res$K2
			cov_exp[ii,jj] <- cov_exp[jj,ii] <- res$cov
			cor_exp[ii,jj] <- cor_exp[jj,ii] <- res$cor			
			chisq[ii,jj] <- chisq[jj,ii] <- chisq_compute(obs=biv_obs[ii,jj,,], exp=biv_exp[ii,jj,,])			
			attr(chisq, "df")[ii,jj] <- attr(chisq, "df")[jj,ii] <- (K1-1)*(K2-1)
		}
	}
	diag(chisq) <- NA
	attr(chisq, "p") <- 1 - stats::pchisq(chisq, df=attr(chisq,"df") )
	#--- output
	res <- list( uni_obs=uni_obs, uni_exp=uni_exp, M_obs=M_obs, M_exp=M_exp, SD_obs=SD_obs, 
				SD_exp=SD_exp, biv_obs=biv_obs, biv_exp=biv_exp, biv_N=biv_N,
				cov_obs=cov_obs, cov_exp=cov_exp, cor_obs=cor_obs, cor_exp=cor_exp,
				chisq = chisq )
	return(res)
}
