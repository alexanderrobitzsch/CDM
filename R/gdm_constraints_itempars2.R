## File Name: gdm_constraints_itempars2.R
## File Version: 0.03

##########################################################
# constraints on item parameters
gdm_constraints_itempars2 <- function( b.constraint , a.constraint ,
    K , TD ,I , dat )
{
    K.item <- apply( dat , 2 , max )
    for (ii in 1:I){    # ii <- 1
        K.ii <- K.item[ii]
        if ( K.ii < K ){
            for ( kk in (K.ii+1):K){
                b.constraint <- rbind( b.constraint , cbind( ii , kk , -99999 ) )
                for (td in 1:TD){
                    a.constraint <- rbind( a.constraint , cbind( ii , td , kk , 0 ) )
                }
            }
        }
    }
    res <- list(K.item=K.item , a.constraint=a.constraint ,
            b.constraint = b.constraint )
    return(res)
    }
###############################################################
