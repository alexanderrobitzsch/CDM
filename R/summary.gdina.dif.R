## File Name: summary.gdina.dif.R
## File Version: 0.02


summary.gdina.dif <- function(object,...)
{
    stats <- object$difstats
    for (vv in 2:ncol(stats) ){
        stats[,vv] <- round(stats[,vv], digits=4)
    }
    print(stats)
}
