## File Name: itemfit_sx2_calc_scoredistribution_R.R
## File Version: 0.01


##########################################################################
# calculate distribution of sum score
itemfit_sx2_calc_scoredistribution_R <- function( pjk )
{
    # pjk .... [ TP , I , 2 ]   ... [ theta points , items , 2 categories ]    
    P1 <- pjk[,,2]
    Q1 <- pjk[,,1]
    TP <- nrow(P1)
    I <- ncol(P1)
    score <- seq( 0 , I , 1 )
    scoredistribution <- matrix(NA , TP , I+1 )
    scoredistribution[,1] <- Q1[,1]
    scoredistribution[,2] <- P1[,1]	
    for (ii in 2:I){
        scoredistribution0 <- scoredistribution
        scoredistribution[,ii+1] <- P1[,ii] * scoredistribution0[,ii]
        for (kk in seq( 0 , ii - 2 , 1 ) ){
            scoredistribution[,ii-kk] <- Q1[,ii] * scoredistribution0[,ii-kk] + 
					P1[,ii] * scoredistribution0[,ii-kk-1]
        }
        scoredistribution[,1] <- Q1[,ii] * scoredistribution0[,1]
    }			
    return(scoredistribution)
}
##############################################################################

.calc.scoredistribution.cdm <- itemfit_sx2_calc_scoredistribution_R
