## File Name: plot_gdm_cor_numbers.R
## File Version: 0.02


#- correlations between dimensions
plot_gdm_cor_numbers <- function( cor.trait, dim1, dim2, cexcor)
{
    graphics::plot( c(0,1), c(0,1), type="n", axes=FALSE, xlab="", ylab="")
    graphics::text( .5, .50, paste0( round( cor.trait[dim1,dim2],3)), cex=cexcor)
}
