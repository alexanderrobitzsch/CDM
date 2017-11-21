## File Name: equivalent.dina.R
## File Version: 2.14
equivalent.dina <- function (q.matrix, reparametrization = "B") 
{
	K <- ncol(q.matrix)
	I <- nrow(q.matrix)
	if (is.null(colnames(q.matrix))) {
		colnames(q.matrix) <- paste("S", 1:K, sep = "")
	}
	if (is.null(rownames(q.matrix))) {
		rownames(q.matrix) <- paste("I", 1:I, sep = "")
	}
	L <- 2^K
	
	dich_vec <- c(0,1)
	dfr <- as.matrix( dich_vec , ncol=1)
	rownames(dfr) <- NULL
	for (kk in 2:K){
		dfr <- rbind( cbind( dfr , 0) , cbind( dfr , 1) )
	}
	attr.patt <- as.matrix(dfr)

	alpha <- attr.patt
	l1 <- apply(alpha, 1, FUN = function(hh) {
			paste(hh, collapse = "")
		})
	alpha <- matrix(NA, L, K)
	for (kk in 1:K) {
		alpha[, kk] <- as.numeric(substring(l1, kk, kk))
	}
	alpha <- data.frame(alpha)
	rownames(alpha) <- l1
	colnames(alpha) <- colnames(q.matrix)
	
	#*** link to alpha.ast
	alpha_ast_index <- alpha[,1]
	for ( kk in 2:K){
		alpha_ast_index <- alpha_ast_index + 2^(kk-1)*alpha[,kk]
	}
	
	#--- reparametrization B	
	if (reparametrization == "B") {
		qclasses <- q.matrix[,1]
		for (kk in 2:K){
			qclasses <- qclasses + 2^(kk-1) * q.matrix[,kk]
		}
		uqclasses <- setdiff(unique(qclasses),0)
		qclasses <- match( qclasses ,  uqclasses )
		L1 <- length( uqclasses )
		q.matrix.ast <- matrix(0, I, L1)
		q.matrix.ast[ cbind(1:I , qclasses) ] <- 1
		qclasses[ is.na(qclasses) ] <- -9
		rownames(q.matrix.ast) <- rownames(q.matrix)
		v1 <- rep( "" , L1 )	
		for (cc in 1:L1){
			q_cc <- q.matrix[ which(qclasses == cc) ,  , drop=FALSE]
			q_cc <- q_cc[1,]
			v1[cc] <- paste0("S*" , paste0( q_cc , collapse= "" ) )
		}
		colnames(q.matrix.ast) <- v1
		alpha.ast <- matrix(0, L, L1)
		rownames(alpha.ast) <- rownames(alpha)
		colnames(alpha.ast) <- colnames(q.matrix.ast)
		for (tt in 1:L){
			alpha_tt <- alpha[ tt , ]
			for (ll in 1:L1){
				q_ll <- q.matrix[ qclasses == ll , , drop=FALSE]
				q_ll <- q_ll[1,]
				v_tt_ll <- prod( alpha_tt^q_ll )
				if (v_tt_ll == 1 ){
					alpha.ast[tt,ll] <- 1
				}
			}
		}
	}
	#----------------------------------------------
	#--- reparametrization A
	if (reparametrization == "A") {
		alpha.ast <- matrix(0, L, L)
		rownames(alpha.ast) <- rownames(alpha)
		colnames(alpha.ast) <- rownames(alpha)
		diag(alpha.ast) <- 1
		alpha.ast <- alpha.ast[, -1]
		q.matrix.ast <- matrix(0, I, L)
		rownames(q.matrix.ast) <- rownames(q.matrix)
		colnames(q.matrix.ast) <- paste("S*", rownames(alpha), sep = "")
		for (ii in 1:I) {
			q.ii <- q.matrix[ii, ]
			a1 <- rep(0,L)
			for (hh in 1:L){			
				v_hh <- prod( alpha[hh, ]^q.ii )
				if ( v_hh == 1 ){
					a1[hh] <- 1
				}
			}
			q.matrix.ast[ii, ] <- a1			
		}
		q.matrix.ast[ rowSums(q.matrix) == 0 , ] <- 0
		q.matrix.ast <- q.matrix.ast[, -1]
	}
	#--- OUTPUT
	res <- list(q.matrix = q.matrix, q.matrix.ast = q.matrix.ast,
					alpha=alpha, alpha.ast=alpha.ast )
	return(res)
}
