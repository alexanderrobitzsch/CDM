## File Name: gdina_init_class_probabilities.R
## File Version: 0.07

gdina_init_class_probabilities <- function( G, L, seed, attr.prob.init )
{
    if (G==1){
        attr.prob <- rep( 1/L, L )
        if ( seed > 0 ){
            set.seed(seed)
            attr.prob <- cdm_sumnorm( stats::runif(L) )
        }
    } else {
        attr.prob <- matrix( 1/L, L, G )
        if ( seed > 0 ){
            set.seed(seed)
            for (gg in 1:G){
                attr.prob[,gg] <- cdm_sumnorm( stats::runif(L) )
            }
        }
    }
    #--- initial class probabilities
    if ( ! is.null(attr.prob.init) ){
        attr.prob <- attr.prob.init
        if (G==1){
            attr.prob <- as.vector(attr.prob)
        }
    }
    #---- OUTPUT
    res <- list(attr.prob=attr.prob)
    return(res)
}
