## File Name: plot_gdm_hist.R
## File Version: 0.01



#-- histogram plot
plot_gdm_hist <- function( theta.k, pi.k, object, dim, group, barwidth, histcol,
                mean.trait, sd.trait )
{
    dd <- dim
    a1 <- stats::aggregate( pi.k, list( theta.k[, dd] ), sum )
    mainpl <- paste0("Dim", dd, " | M=", round( mean.trait[dd], 3 ),
                        " | SD=",round( sd.trait[dd], 3 ) )
    graphics::plot( a1[,1], a1[,2], type="n", xlab=paste0("theta (Dim", dd, ")" ),
                        ylab="Probability", main=mainpl)
    AA <- nrow(a1)
    for ( aa in 1:AA){
        graphics::rect(xleft=a1[aa,1] - barwidth/2, ybottom=0,
                xright=a1[aa,1] + barwidth/2, ytop=a1[aa,2], col=histcol)
    }
}

