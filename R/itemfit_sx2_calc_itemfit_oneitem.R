## File Name: itemfit_sx2_calc_itemfit_oneitem.R
## File Version: 0.01


#################################################
# calculation of item fit for one item
itemfit_sx2_calc_itemfit_oneitem <- function( ii , pjk , pi.k , P1 , I , Eik_min ,
         sumscore.distribution , scoredistribution , data , sumscore )
{
    eps <- 1E-10
	pjk.ii <- pjk[,-ii,]	
    P1ii <- pjk.ii[,,2]
    Q1ii <- pjk.ii[,,1]
	scored.ii <- cdm_rcpp_itemfit_sx2_calc_scoredistribution( P1=P1ii, Q1=Q1ii )
    eik_t2 <- colSums( scoredistribution * pi.k )
    eik_t1 <- c(0,colSums( P1[,ii] * scored.ii * pi.k  ) )
	# P1 is the probability of passing the item
    eik <- eik_t1 / eik_t2
	N <- nrow(data)
    oik <- sapply( 0:I , FUN = function(ss){ mean( data[ sumscore == ss , ii ] ) } )
    dfr.ii <- data.frame( "item" = colnames(data)[ii] , "itemindex" = ii , 
                "score" = 0:I , 
                "Nik" = sumscore.distribution ,
                "oik" = oik , "eik" = eik )
    dfr.ii$eik_t1 <- eik_t1
    dfr.ii$eik_t2 <- eik_t2
    dfr.ii$Eik <- N * eik_t2
    dfr.ii[ 1 , "oik" ] <- 0
    dfr.ii[ is.na(dfr.ii) ] <- 0
    #******************
    # merging of categories
    x1 <- floor( I/2 )
    dfr2.ii <- NULL
    mm1 <- 2
	
	#** from below
    while( mm1 <= x1 ){
        t1 <- 0
        ss <- mm1-1
        while( t1 < Eik_min){
            ss <- ss + 1
            t1 <- t1 + dfr.ii[ ss , "Eik" ]
                }
        mm2 <- ss
        dfr2vv <- dfr.ii[ mm1:mm2 , ]
        dfr2.iivv <- data.frame(  "item" = colnames(data)[ii] , "itemindex" = ii )
        dfr2.iivv$score.min <- min( dfr2vv$score )
        dfr2.iivv$score.max <- max( dfr2vv$score )
        dfr2.iivv$score <- mean( dfr2vv$score )
        dfr2.iivv$Nik <- sum( dfr2vv$Nik )
        dfr2.iivv$oik <- sum( dfr2vv$Nik*dfr2vv$oik ) / sum( dfr2vv$Nik + eps)
        dfr2.iivv$eik <- sum( dfr2vv$eik_t1 ) / sum( dfr2vv$eik_t2 + eps )
        dfr2.iivv$Eik <- sum( dfr2vv$Eik )
        dfr2.ii <- rbind( dfr2.ii , dfr2.iivv )
        mm1 <- mm2 + 1 
                }
    dfr2a.ii <- dfr2.ii

	#*** from above
    dfr2.ii <- NULL
    mm1 <- I
    while( mm1 > x1 ){
        t1 <- 0
        ss <- mm1+1
        while( t1 < Eik_min){
            ss <- ss - 1
            t1 <- t1 + dfr.ii[ ss , "Eik" ]
                }
    
        mm2 <- ss
        dfr2vv <- dfr.ii[ mm2:mm1 , ]
        dfr2.iivv <- data.frame(  "item" = colnames(data)[ii] , "itemindex" = ii )
        dfr2.iivv$score.min <- min( dfr2vv$score )
        dfr2.iivv$score.max <- max( dfr2vv$score )
        dfr2.iivv$score <- mean( dfr2vv$score )
        dfr2.iivv$Nik <- sum( dfr2vv$Nik )
        dfr2.iivv$oik <- sum( dfr2vv$Nik*dfr2vv$oik ) / sum( dfr2vv$Nik + eps)
        dfr2.iivv$eik <- sum( dfr2vv$eik_t1 ) / sum( dfr2vv$eik_t2 + eps )
        dfr2.iivv$Eik <- sum( dfr2vv$Eik )
        dfr2.ii <- rbind( dfr2.ii , dfr2.iivv )
        mm1 <- mm2 - 1 
                }
    dfr2.ii <- rbind( dfr2a.ii , dfr2.ii )
    dfr2.ii <- dfr2.ii[ order( dfr2.ii$score) , ]
    res <- list( "table1.ii" = dfr.ii , "table2.ii" = dfr2.ii )
    return(res)
}
##########################################################################
                
.calc.itemfit.oneitem <- itemfit_sx2_calc_itemfit_oneitem
				
