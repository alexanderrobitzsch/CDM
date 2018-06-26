## File Name: mcdina_calc_se_delta.R
## File Version: 0.01

# standard errors mcdina
mcdina_calc_se_delta <- function( delta, n.ik, probs, lr_list, lc_list,
            itemstat, I, G, itempars, lr_counts, CC )
{
    se.delta <- delta
    for (ii in 1:I){
        for (gg in 1:G){
            lc.ii <- lc_list[[ii]]
            lr.ii <- lr_list[[ii]]
            se.delta[ii,,,gg] <- sqrt( delta[ii,,,gg] * ( 1 - delta[ii,,,gg] ) /
                    matrix( lr_counts[ii,,gg], nrow=CC, ncol=CC, byrow=TRUE) )
        } # end gg
    } # end ii
    return(se.delta)
}
