## File Name: gdina_attribute_structure_hogdina.R
## File Version: 0.09

gdina_attribute_structure_hogdina <- function(G, attr.prob, attr.patt, wgt.theta ,
            HOGDINA, a.attr, b.attr, theta.k, tetrachoric )
{
    tetrachoric0 <- list()
    for (gg in 1:G){
        if (G==1){ ap.gg <- attr.prob
        } else {
            ap.gg <- attr.prob[,gg]
        }
        tetrachoric_init <- NULL
        if ( ! is.null(tetrachoric ) ){
            tetrachoric_init <- tetrachoric[[gg]]
        }
        res <- gdina_attr_rpf_hogdina( attr.patt=attr.patt , attr.prob=ap.gg , theta.k=theta.k ,
                    wgt.theta=wgt.theta[,gg] , HOGDINA=HOGDINA, tetrachoric=tetrachoric_init )
        tetrachoric0[[gg]] <- res$tetrachoric
        if (G==1){
            attr.prob <- res$attr.prob
        } else {
            attr.prob[,gg] <- res$attr.prob
        }
        a.attr[,gg] <- res$a.attr
        b.attr[,gg] <- res$b.attr
    }
    #--- OUTPUT
    res <- list( a.attr=a.attr, b.attr=b.attr, attr.prob=attr.prob,
                    tetrachoric = tetrachoric0 )
    return(res)
}
