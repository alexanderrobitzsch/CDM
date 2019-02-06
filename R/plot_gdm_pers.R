## File Name: plot_gdm_pers.R
## File Version: 0.01


# plot person parameters
plot_gdm_pers <- function( person, dim1, dim2, pchpers, cexpers, perstype )
{
    dd1 <- dim1
    dd2 <- dim2
    graphics::plot( person[,dd1], person[,dd2], xlab=paste0(perstype, " Dim",dd1),
            ylab=paste0(perstype, " Dim",dd2), pch=pchpers, cex=cexpers)
}
