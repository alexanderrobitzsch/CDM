## File Name: plot.gdm.R
## File Version: 0.15



#** S3 plot method for gdm function
plot.gdm <- function( x, perstype="EAP", group=1, barwidth=.1, histcol=1,
        cexcor=3, pchpers=16, cexpers=.7, ... )
{
    object <- x
    theta.k <- object$theta.k
    pi.k <- object$pi.k[,group]
    Ndim <- ncol(theta.k)
    mean.trait <- object$mean.trait[,group]
    sd.trait <- object$sd.trait[,group]
    cor.trait <- object$correlation.trait[[group]]
    # extract person parameters
    person <- object$person[ object$group==group, ]
    person <- person[, grep( paste0(perstype,"."), colnames(person) ) ]

    # define plot grid
    plotgrid <- as.data.frame( expand.grid( 1:Ndim, 1:Ndim )[,c(2,1) ] )
    plotgrid$type <- ""
    plotgrid[ plotgrid[,1]==plotgrid[,2], "type" ] <- "hist"
    plotgrid[ plotgrid[,1] < plotgrid[,2], "type" ] <- "cornumber"
    plotgrid[ plotgrid[,1] > plotgrid[,2], "type" ] <- "scatterEAP"
    PG <- nrow(plotgrid)

    graphics::par( mfrow=c(Ndim,Ndim) )
    for (pp in 1:PG){
        if ( paste(plotgrid$type)[pp]=="cornumber"){
            plot_gdm_cor_numbers( cor.trait=cor.trait, dim1=plotgrid[pp,1], 
                    dim2=plotgrid[pp,2], cexcor=cexcor)
        }
        if ( paste(plotgrid$type)[pp]=="scatterEAP"){
            plot_gdm_pers( person=person, dim1=plotgrid[pp,1], dim2=plotgrid[pp,2], 
                    pchpers=pchpers, cexpers=cexpers, perstype=perstype )
        }
        if ( paste(plotgrid$type)[pp]=="hist"){
            plot_gdm_hist( theta.k=theta.k, pi.k=pi.k, object=object, dim=plotgrid[pp,1], 
                        group=group, barwidth=barwidth, histcol=histcol, 
                        mean.trait=mean.trait, sd.trait=sd.trait ) 
        }
    }
    graphics::par( mfrow=c(1,1))
}

