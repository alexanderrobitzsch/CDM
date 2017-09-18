## File Name: gdina_attribute_structure_hogdina.R
## File Version: 0.03
## File Last Change: 2017-06-04 19:26:04

gdina_attribute_structure_hogdina <- function(G, attr.prob, attr.patt, wgt.theta ,
			HOGDINA, a.attr, b.attr, theta.k )
{
    for (gg in 1:G){ # gg <- 1
		if (G==1){ ap.gg <- attr.prob 
		} else {
			ap.gg <- attr.prob[,gg] 
		}
		res <- gdina_attr_rpf_hogdina( attr.patt=attr.patt , attr.prob=ap.gg , theta.k=theta.k , 
					wgt.theta=wgt.theta[,gg] , HOGDINA=HOGDINA )
		if (G==1){ attr.prob <- res$attr.prob 
		} else {
			attr.prob[,gg] <- res$attr.prob 
		}
		a.attr[,gg] <- res$a.attr
		b.attr[,gg] <- res$b.attr 
	}
	#--- OUTPUT
	res <- list( a.attr=a.attr, b.attr=b.attr, attr.prob=attr.prob)
	return(res)
}
