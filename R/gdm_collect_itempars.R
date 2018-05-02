## File Name: gdm_collect_itempars.R
## File Version: 0.02



################################################################
# collect item parameters
gdm_collect_itempars <- function( data , K , D , b , a , TD , thetaDes , irtmodel ,
            se.b , se.a , data0)
{
    # collect item parameters
    item <- data.frame( "item" = colnames(data0) ,
                    "N" = colSums(1-is.na(data0) ) )
    item$M <- colMeans(data0 , na.rm=T)
    # b parameters
    se.b[ b < -9999  ] <- NA
    b[ b < -9999  ] <- NA
    se.a[ a < -9999  ] <- NA
    a[ a < -9999  ] <- NA

    for (kk in 1:K){
        item[ , paste0( "b.Cat" , kk) ] <- b[,kk]
    }
    for (dd in 1:TD){
        if ( irtmodel %in% c("1PL" , "2PL") ){
            item[ , paste0( "a." , colnames(thetaDes)[dd] ) ] <- a[,dd,1]
        }
        if ( irtmodel %in% c("2PLcat") ){
            for (kk in 1:K){
                item[ , paste0( "a." , colnames(thetaDes)[dd] , ".Cat", kk ) ] <- a[,dd,kk]
            }
            se.a[ a == -99999  ] <- NA
            a[ a == -99999  ] <- NA
        }
    }
    #--- OUTPUT
    res <- list(item = item , b = b , se.b = se.b , a = a )
    return(res)
}

.gdm.collect.itempars <- gdm_collect_itempars
