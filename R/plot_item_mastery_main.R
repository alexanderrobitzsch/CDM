## File Name: plot_item_mastery_main.R
## File Version: 0.23

plot_item_mastery_main <- function(object, pch=c(16,17), lty=c(1,2), ...)
{
    probs <- IRT.irfprob(object)
    irf_dim <- dim(probs)
    I <- irf_dim[1]
    K <- irf_dim[2]
    TP <- irf_dim[3]
    if (K>2){ 
        stop("Plot function can only be used for dichotomous data.\n")
    }
    graphics::plot(c(1,I), c(0,1.1), type="n", xlab="Item index", ylab="Probability", 
                axes=FALSE, ...)
    if (I <= 10){ item_by <- 1}
    if (I > 10){ item_by <- 2}
    if (I > 24){ item_by <- 3}
    if (I >= 50){ item_by <- 5}                            
    item_labels <- seq(1,I,by=item_by)
    graphics::axis(1, at=item_labels)
    graphics::axis(2, at=seq(0,1,.2) )
    graphics::abline(h=1, col="gray")
    probs_ii <- probs[,2,]    
    for (ll in 1:2){
        if (ll==1){ ind_ll <- 1 } else { ind_ll <- TP }
        prob_plot <- probs_ii[,ind_ll]
        graphics::lines(1:I, prob_plot, lty=lty[ll])
        graphics::points(1:I, prob_plot, pch=pch[ll])
    }
    graphics::legend(x="top", c("Non-masters", "Masters"), lty=lty, pch=pch, horiz=TRUE)
}

plot_item_mastery.din <- plot_item_mastery_main
plot_item_mastery.gdina <- plot_item_mastery_main
