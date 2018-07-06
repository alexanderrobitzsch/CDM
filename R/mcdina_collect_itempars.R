## File Name: mcdina_collect_itempars.R
## File Version: 0.04


# collect item parameters
mcdina_collect_itempars <- function( I, lc, itempars, itemstat, dat,
    G, CC, delta, se.delta, group0_unique  )
{
    item <- NULL
    for (ii in 1:I){
        lc.ii <- lc[ lc$item==ii, ]
        ip.ii <- itempars[ii]
        itemstat.ii <- itemstat[ii,]
        if ( ip.ii=="gr" ){
            G1 <- G
        } else {
            G1 <- 1
        }
        for (gg in 1:G1){  #  gg <- 1
            delta.ii <- delta[ ii,,,gg ]
            se.delta.ii <- se.delta[ ii,,,gg ]
            for (cc in 1:itemstat.ii$N.lr ){
                lc.ii.cc <- lc.ii[ lc.ii$lr_index==cc, ]
                lc.ii.cc <- lc.ii.cc[1,]
                item.cc <- data.frame( "item"=colnames( dat )[ii], "itemnr"=lc.ii.cc$item )
                item.cc$lr <- lc.ii.cc$lr
                item.cc$lr_level <- lc.ii.cc$lr_level
                item.cc$Q <- lc.ii.cc$Q
                item.cc$lr_index <- lc.ii.cc$lr_index
                item.cc$max.cat <- lc.ii.cc$max.cat
                item.cc$partype <- itemstat.ii$partype
                item.cc$group <- group0_unique[gg]
                if ( item.cc$partype !="gr" ){ item.cc$group <- NA }
                d1 <- t( delta.ii[,cc] )
                colnames(d1) <- paste0( "Cat", 1:CC )
                if ( itemstat.ii$N.cat < CC ){
                    d1[ 1, seq(itemstat.ii$N.cat + 1, CC ) ] <- NA
                }
                item.cc <- cbind( item.cc, d1 )
                d1 <- t( se.delta.ii[,cc] )
                if ( itemstat.ii$N.cat < CC ){
                d1[ 1, seq(itemstat.ii$N.cat + 1, CC ) ] <- NA
                }
                colnames(d1) <- paste0( "se.Cat", 1:CC )
                item.cc <- cbind( item.cc, d1 )
                item <- rbind( item, item.cc )
            }
        }
    }
    item <- item[ ! is.na(item$itemnr), ]
    return(item)
}
