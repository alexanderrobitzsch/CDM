## File Name: gdina_designmatrices_create_Mj_general.R
## File Version: 0.02



gdina_designmatrices_create_Mj_general <- function( K , Aj , rule, bugs=NULL, q_jj = NULL )
{
    Mj <- cbind( 1 , sapply( 1:K , FUN = function(jj){ 1*(Aj[,jj]==1) } ) )
    Mj.lab <- c( "0" , 1:K )
    if (K>1){
        for (kk in 2:K){
            g2 <- utils::combn( K , kk )
            if (kk==K){
                g2 <- matrix( g2 , nrow=length(g2) , ncol=1 )
            }
            Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
            Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
        }
    } else {
        Mj.lab <- c( "0" , "1" )
    }
    
    if (rule == "DINA"){
        M <- 2^K
        selv <- c(1,M)
        Mj <- Mj[,selv]
        Mj.lab <- Mj.lab[  selv]
    }
    if (rule == "DINO"){
        M <- 2^K
        selv <- c(1,M)
        Mj <- Mj[,selv]
        Mj[,2] <- 1
        Mj[1,2] <- 0
        Mj.lab <- Mj.lab[ selv]
        Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
    }
    if (rule == "ACDM" | rule == "GDINA1"){
        selv <- 1:(K+1)
        Mj <- Mj[,selv]
        Mj.lab <- Mj.lab[  selv ]
    }
    if (rule == "GDINA2"){
        selv <- 1:( 1 + K + K*(K-1)/2 )
        Mj <- Mj[,selv]
        Mj.lab <- Mj.lab[  selv ]
    }
    #**** SISM model
    if (rule == "SISM"){
        Q_entries <- which( q_jj > 0)
        K_jj1 <- length(q_jj)            
        skills <- setdiff( 1:K_jj1, bugs )
        skills_jj <- intersect( skills, Q_entries )
        bugs_jj <- intersect( bugs, Q_entries )
        if( length(skills_jj) > 0){
            ind_skills <- seq(1, length(skills_jj),1 )
        } else {
            ind_skills <- NULL
        }
        if( length(bugs_jj) > 0){
            ind_bugs <- length(ind_skills) + seq(1, length(bugs_jj),1 )
        } else {
            ind_bugs <- NULL
        }
        
        NS <- length(ind_skills)
        NB <- length(ind_bugs)
                
        Mj <- 0*Mj
        is_sism <- FALSE
        if ( (NS>0) & (NB>0) ){
            Mj <- Mj[ , 1:4]
            Mj.lab <- c("S0B1", "S0B0", "S1B1", "S1B0")
            is_sism <- TRUE
        } else {
            Mj <- Mj[,1:2]
            if (NS>0){
                Mj.lab <- c("S0","S1")
            }
            if (NB>0){
                Mj.lab <- c("B1","B0")
            }
        }    
                
        #*** latent response corresponding to skills
        h1 <- 0    
        if (NS>1){ h1 <- rowMeans( Aj[, ind_skills] ) }
        if (NS==1){ h1 <- Aj[, ind_skills] }    
        latresp_eta <- 1*( h1 == 1 )
        
        #*** latent response bugs
        h1 <- 0
        if (NB>1){h1 <- rowSums(Aj[,ind_bugs]) }
        if (NB==1){    h1 <- Aj[,ind_bugs] }

        latresp_gamma <- 1 * ( h1 > 0 )
        
        if (is_sism){
            Mj[ ( latresp_eta == 0 ) & ( latresp_gamma == 1) , 1 ] <- 1
            Mj[ ( latresp_eta == 0 ) & ( latresp_gamma == 0) , 2 ] <- 1
            Mj[ ( latresp_eta == 1 ) & ( latresp_gamma == 1) , 3 ] <- 1
            Mj[ ( latresp_eta == 1 ) & ( latresp_gamma == 0) , 4 ] <- 1
        } else {
            if (NS>0){
                Mj[ latresp_eta == 0 , 1 ] <- 1
                Mj[ latresp_eta == 1 , 2 ] <- 1            
            }
            if (NB>0){
                Mj[ latresp_gamma == 1 , 1 ] <- 1
                Mj[ latresp_gamma == 0 , 2 ] <- 1            
            }        
        }
    }

    res <- list( Mj = Mj , Mj.lab = Mj.lab )
    return(res)
}
