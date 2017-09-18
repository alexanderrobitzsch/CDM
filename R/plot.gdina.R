## File Name: plot.gdina.R
## File Version: 0.03
## File Last Change: 2017-01-31 14:07:29



plot.gdina <- function( x , ask=FALSE , ... ){
    probitem <- x$probitem
    I <- max( probitem$itemno )
    for (ii in 1:I){
        # ii <- 1
        pii <- probitem[ probitem$itemno == ii , ]
        graphics::barplot( pii$prob  , ylim=c(0,1) , xlab="Skill Pattern" ,
            names.arg =pii$skillcomb ,
            main = paste0( "Item " , pii[1,"item" ] , " (Rule " , x$rule[ii] , ")\n" ,
                    "Attributes " , pii[1,"partype.attr"] ) , ask=ask 
                        )
                }
         }
