## File Name: reglca_monitor_random_starts.R
## File Version: 0.06

reglca_monitor_random_starts <- function( control_random_starts, iter, opt_fct, item_probs,
        class_probs, max_increment0, max_increment, item_probs_random_starts, class_probs_random_starts )
{
    if ( control_random_starts$use_random_starts ){
        random_starts <- control_random_starts$random_starts
        if (iter > control_random_starts$random_iter ){
            rr0 <- control_random_starts$random_start_temp
            control_random_starts$opt_fct[rr0] <- opt_fct
            control_random_starts$item_probs[[rr0]] <- item_probs
            control_random_starts$class_probs[[rr0]] <- class_probs
            control_random_starts$max_increment[[rr0]] <- max_increment
            rr <- rr0 + 1
            if (rr > random_starts){
                control_random_starts$use_random_starts <- FALSE
                ind <- which.min(control_random_starts$opt_fct)[1]
                item_probs <- control_random_starts$item_probs[[ind]]
                class_probs <- control_random_starts$class_probs[[ind]]
                max_increment <- control_random_starts$max_increment[[ind]]
                iter <- control_random_starts$random_iter + 1
            } else {
                control_random_starts$random_start_temp <- rr
                item_probs <- item_probs_random_starts[[rr]]
                class_probs <- class_probs_random_starts[[rr]]
                max_increment <- max_increment0
                iter <- 1
            }

        }
    }
    #---- output
    res <- list(control_random_starts=control_random_starts, max_increment=max_increment,
                    iter=iter, item_probs=item_probs, class_probs=class_probs)
    return(res)
}
