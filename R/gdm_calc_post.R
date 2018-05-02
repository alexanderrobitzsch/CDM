## File Name: gdm_calc_post.R
## File Version: 0.03


###############################################################
# calculation of posterior probabilities
gdm_calc_post <- function(pi.k, group, p.xi.aj, weights, G, ind.group,
        use.freqpatt )
{
        # posterior probabilities  P( \alpha_l | X_i )
    sel <- 1
    if ( use.freqpatt & (G>1) ){
        sel <- 2
    }
    #*****************
    if ( sel == 1 ){
        prior <- ( t( pi.k ) )[ group , ]
        p.aj.xi <- prior * p.xi.aj
        p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )
        # calculate pi.k
        for (gg in 1:G){ # gg <- 1
            ind.gg <- ind.group[[gg]]
            wgg <- weights[ind.gg]
            if (G==1){
                pi.k[,gg] <- colSums( p.aj.xi * wgg ) / sum( wgg )
            }
            if (G>1){
                pi.k[,gg] <- colSums( p.aj.xi[ ind.gg , ] * wgg ) / sum( wgg )
            }
        }
    }
    #***********************
    if ( sel == 2 ){  # if use.freqpatt == TRUE for multiple groups
        # calculate pi.k
        p.aj.xi <- list(1:G)
        for (gg in 1:G){ # gg <- 1
            wgg <- weights[,gg]
            ind.gg <- which( wgg > 0 )
            NP <- length(ind.gg)
            wgg <- wgg[ind.gg]
            prior <- ( t( pi.k[,gg] ) )[ rep(1,NP) , ]
            p.aj.xi.gg <- prior * p.xi.aj[ind.gg,]
            p.aj.xi.gg <- p.aj.xi.gg / rowSums( p.aj.xi.gg )
            p.aj.xi[[gg]] <- p.aj.xi.gg
            pi.k[,gg] <- colSums( p.aj.xi.gg * wgg ) / sum( wgg )
        }
    }
    #----- OUTPUT
    res <- list("pi.k"=pi.k , "p.aj.xi"=p.aj.xi )
    return(res)
}


.gdm.calc.post <- gdm_calc_post
