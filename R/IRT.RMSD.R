## File Name: IRT.RMSD.R
## File Version: 0.40

IRT.RMSD <- function( object )
{
    CALL <- match.call()
    mod <- object
    mod_counts <- IRT.expectedCounts(mod)
    G <- attr(mod_counts, "G" )
    #--- item response probabilities
    mod_irfprob <- IRT.irfprob(mod)
    prob.theta <- attr(mod_irfprob, "prob.theta")
    prob.theta <- matrix( prob.theta, ncol=G )
    attr(mod_irfprob, "prob.theta") <- prob.theta
    I <- dim(mod_counts)[1]
    #*** create matrix with results
    RMSD <- matrix( NA, nrow=I, ncol=G+1)
    RMSD <- as.data.frame(RMSD)
    colnames(RMSD) <- c("item", paste0("Group", 1:G) )
    RMSD$item <- dimnames(mod_counts)[[1]]
    RMSD_bc <- chisquare_stat <- MD <- MAD <- RMSD
    RMSD$WRMSD <- NULL
    # sample sizes per group
    weight_group <- matrix( NA, nrow=I, ncol=G )
    for (gg in 1:G){
        mc_gg <- apply( mod_counts[,,,gg], c(1), sum )
        weight_group[,gg] <- as.vector( mc_gg )
    }
    weight_group <- t( apply( weight_group, 1, FUN=function(ww){
                            ww / sum(ww) } ) )
    # weight_group <- weight_group / sum( weight_group )

    #*** extract objects
    for (gg in 1:G){
        pi.k <- attr(mod_irfprob, "prob.theta")[, gg, drop=FALSE ]
        probs <- aperm( mod_irfprob, perm=c(3,1,2) )
        n.ik <- aperm( mod_counts, perm=c(3,1,2,4) )[,,,gg,drop=FALSE]
        #*** chi square calculation
        chisquare_stat[,gg+1] <- rmsd_chisquare( n.ik=n.ik, pi.k=pi.k, probs=probs )
        #*** RMSD calculations
        res0 <- IRT_RMSD_calc_rmsd( n.ik=n.ik, pi.k=pi.k, probs=probs )
        RMSD[,gg+1] <- res0$RMSD   # RMSD
        RMSD_bc[,gg+1] <- res0$RMSD_bc   # RMSD_bc
        #----
        md_gg <- IRT_RMSD_calc_md( n.ik=n.ik, pi.k=pi.k, probs=probs )
        MD[,gg+1] <- md_gg
        # MD[,gg+1] <- res0$MD      # MD
        MAD[,gg+1] <- res0$MAD   # MAD
    }

    M1 <- rowSums( RMSD[,2:(G+1) ]^2 * weight_group )
    RMSD$WRMSD <- sqrt( M1 )
    M1 <- rowSums( RMSD_bc[,2:(G+1) ]^2 * weight_group )
    RMSD_bc$WRMSD <- sqrt( M1 )
    if ( G==1 ){
        RMSD$WRMSD <- NULL
        RMSD_bc$WRMSD <- NULL
    }

    #*** summaries of statistics
    RMSD_summary <- dataframe_summary( dfr=RMSD, exclude_index=1,
                        labels=colnames(RMSD)[-1] )
    RMSD_bc_summary <- dataframe_summary( dfr=RMSD_bc, exclude_index=1,
                        labels=colnames(RMSD_bc)[-1] )
    MD_summary <- dataframe_summary( dfr=MD, exclude_index=1,
                        labels=colnames(MD)[-1] )
    MAD_summary <- dataframe_summary( dfr=MAD, exclude_index=1,
                        labels=colnames(MAD)[-1] )

    #*** output
    res <- list( MD=MD, RMSD=RMSD, RMSD_bc=RMSD_bc, MAD=MAD,
                    chisquare_stat=chisquare_stat,
                    call=CALL, G=G, RMSD_summary=RMSD_summary,
                    RMSD_bc_summary=RMSD_bc_summary,    MD_summary=MD_summary,
                    MAD_summary=MAD_summary)
    class(res) <- "IRT.RMSD"
    return(res)
}
