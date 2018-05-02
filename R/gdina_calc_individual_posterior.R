## File Name: gdina_calc_individual_posterior.R
## File Version: 0.14

gdina_calc_individual_posterior <- function(G, IP, attr.prob, p.xi.aj, L, I,
        zeroprob.skillclasses, reduced.skillspace, item.patt.freq)
{
    # posterior probabilities  P( \alpha_l | X_i )
    if (G==1){
        p.aj.xi <- cdm_matrix2( x=attr.prob , nrow=IP) * p.xi.aj
    } else {
        p.aj.xi <- array( 0 , c( IP , L , G ) )
        for (gg in 1:G){
            p.aj.xi[,,gg] <- cdm_matrix2( x= as.vector(attr.prob[,gg]) , nrow=IP) * p.xi.aj
        }
    }

    if (G == 1){
        if ( ! is.null( zeroprob.skillclasses ) ){
            p.aj.xi[ , zeroprob.skillclasses ] <- 0
        }
        p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )
        # calculate marginal probability P(\alpha_l) for attribute alpha_l
        if ( ! reduced.skillspace ){
            attr.prob <- colSums( p.aj.xi * item.patt.freq ) / I
        }
    }
    if ( G > 1 ){
        if ( ! is.null( zeroprob.skillclasses ) ){
            for (gg in 1:G){
                p.aj.xi[ , zeroprob.skillclasses , gg ] <- 0
            }
        }
        for( gg in 1:G){
            p.aj.xi[,,gg] <- p.aj.xi[,,gg] / rowSums( p.aj.xi[,,gg] )
            Igg <- sum( item.patt.freq[,gg] )
            attr.prob[,gg] <- colSums( p.aj.xi[,,gg] * item.patt.freq[,gg] ) / Igg
        }
    }
    #---- OUTPUT
    res <- list( p.aj.xi=p.aj.xi, attr.prob=attr.prob)
    return(res)
}
