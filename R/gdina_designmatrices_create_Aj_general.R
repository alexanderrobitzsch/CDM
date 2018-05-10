## File Name: gdina_designmatrices_create_Aj_general.R
## File Version: 0.01


gdina_designmatrices_create_Aj_general <- function(nq)
{
    Aj <- matrix( rep(0,nq) , nrow=1 , ncol=nq )
    for (kk in 1:nq){
        av <- t( utils::combn( nq , kk ) )
        Aj1 <- matrix( 0 , nrow= nrow(av) , ncol=nq )
        for (hh in 1:kk){
            Aj1[ cbind( seq( 1 , nrow(av) ) , av[,hh]  ) ] <- 1
        }
        Aj <- rbind( Aj , Aj1 )
    }
    return(Aj)
}
