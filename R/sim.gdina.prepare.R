## File Name: sim.gdina.prepare.R
## File Version: 0.05


######################################################################################
# Function for preparation of GDINA simulation
sim.gdina.prepare <- function( q.matrix )
{
    I <- nrow(q.matrix)             # number of items
    rsqm <- rowSums(q.matrix)       # row sums in Q matrix
    necc.attr <- delta <- Mj <- Aj <- as.list( rep(1,I) )
    for (ii in 1:I){
        necc.attr[[ii]] <- which( q.matrix[ii,] > 0 )
        Aj[[ii]] <- gdina_designmatrices_create_Aj( nq=rsqm[ii] )
        Mj[[ii]] <- gdina_designmatrices_create_Mj( Aj=Aj[[ii]], rule="GDINA" )
        delta[[ii]] <- rep( 0, ncol( Mj[[ii]][[1]] ) )
    }
    res <- list( delta=delta, necc.attr=necc.attr, Aj=Aj,  Mj=Mj )
    return(res)
}
######################################################################################
