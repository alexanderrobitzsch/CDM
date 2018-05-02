## File Name: gdina_designmatrices.R
## File Version: 0.34

#****************************************************************
# design matrices for GDINA model
.create.Aj <- function(nq){
    Aj <- NULL
    if (nq == 1){ Aj <- matrix( c(0,1) , ncol=1 ) }
    if (nq == 2){
        Aj <- matrix( c( 0 , 0 ,
                        1 , 0 ,
                        0 , 1 ,
                        1 , 1 ) , byrow=TRUE , ncol=2 )
                    }
    if (nq == 3){
        Aj <- matrix( c( 0 , 0 , 0,
                        1 , 0 , 0 ,    0 , 1 , 0,     0 , 0 , 1 ,
                        1 , 1 , 0 ,    1 , 0 , 1 ,   0 , 1 , 1,
                        1 , 1 , 1 ) , byrow=TRUE , ncol=3 )
                    }
    if (nq == 4){
        Aj <- matrix( c( 0 , 0 , 0, 0,
                        1,0,0,0 ,   0,1,0,0  , 0,0,1,0  , 0,0,0,1 ,
                        1,1,0,0,  1,0,1,0 ,   1,0,0,1,  0,1,1,0 , 0,1,0,1 , 0,0,1,1 ,
                        1,1,1,0 , 1,0,1,1,   1,1,0,1,   0,1,1,1 ,
                        1 , 1 , 1 ,1) , byrow=TRUE , ncol=4 )
                    }
    if (nq == 5){
        Aj <- matrix( c( 0,0,0,0,0   ,
                1,0,0,0,0   ,
                0,1,0,0,0   ,
                0,0,1,0,0   ,
                0,0,0,1,0   ,
                0,0,0,0,1   ,
                1,1,0,0,0   ,
                1,0,1,0,0   ,
                1,0,0,1,0   ,
                1,0,0,0,1   ,
                0,1,1,0,0   ,
                0,1,0,1,0   ,
                0,1,0,0,1   ,
                0,0,1,1,0   ,
                0,0,1,0,1   ,
                0,0,0,1,1   ,
                1,1,1,0,0   ,
                1,1,0,1,0   ,
                1,1,0,0,1   ,
                1,0,1,1,0   ,
                1,0,1,0,1   ,
                1,0,0,1,1   ,
                0,1,1,1,0   ,
                0,1,1,0,1   ,
                0,1,0,1,1   ,
                0,0,1,1,1   ,
                1,1,1,1,0   ,
                1,1,1,0,1   ,
                1,1,0,1,1   ,
                1,0,1,1,1   ,
                0,1,1,1,1   ,
                1,1,1,1,1          ) , byrow=TRUE , ncol=5 )
                    }
    if ( nq > 5){
            Aj <- create_Aj_helper(nq)
                }
    return(Aj)
        }
#*****************************************************************


create_Aj_helper <- function(nq){
    Aj <- matrix( rep(0,nq) , nrow=1 , ncol=nq )
    for (kk in 1:nq){
        # kk <- 1
        av <- t( utils::combn( nq , kk ) )
        Aj1 <- matrix( 0 , nrow= nrow(av) , ncol=nq )
        for (hh in 1:kk){
            Aj1[ cbind( seq( 1 , nrow(av) ) , av[,hh]  ) ] <- 1
                            }
        Aj <- rbind( Aj , Aj1 )
                    }
    return(Aj)
        }

#***************************************************************
# design matrix Mj
.create.Mj <- function( Aj , rule = "GDINA" , q_jj = NULL, bugs=NULL)
{


        K <- ncol(Aj)
        Mj <- NULL
        Mj.lab <- NULL
        #***********************************************
        if (K==1){
            Mj <- matrix( c( 1,0,
                             1 ,1 ) , byrow=TRUE , ncol=2^1 )
            Mj.lab <- c( "0" , "1" )
                    }
        #***********************************************
        if (K==2){
            Mj <- matrix( c( 1,0,0,0,
                             1,1,0,0,    1,0,1,0 ,
                             1 ,1,1,1 ) , byrow=TRUE , ncol=2^2 )
            Mj.lab <- c( "0" , "1" , "2" , "1-2" )
            if (rule == "DINA"){
                M <- 2^2
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[ selv]
                                }
            if (rule == "DINO"){
                M <- 2^2
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj[,2] <- 1 ; Mj[1,2] <- 0
                Mj.lab <- Mj.lab[ selv]
                Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
                                }
            if (rule == "ACDM" | rule == "GDINA1" ){
                selv <- 1:3
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[ selv ]
                                }
            # In case of K = 2, GDINA2 = GDINA
                    }
        #***********************************************
        if (K==3){
            Mj <- cbind( 1 , sapply( 1:3 , FUN = function(jj){ 1*(Aj[,jj]==1) } ) )
               Mj.lab <- c( "0" , 1:3 )
            g2 <- utils::combn( 3 , 2 )
                Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
            g2 <- utils::combn( 3 , 3 )
            g2 <- matrix( g2 , nrow=length(g2) , ncol=1 )
                Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
            if (rule == "DINA"){
                M <- 2^3
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv]
                                }
            if (rule == "DINO"){
                M <- 2^3
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj[,2] <- 1 ; Mj[1,2] <- 0
                Mj.lab <- Mj.lab[ selv]
                Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
                                }

            if (rule == "ACDM" | rule == "GDINA1"){
                selv <- 1:4
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv ]
                                }
            if (rule == "GDINA2"){
                selv <- 1:7
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv ]
                                }
                    }
        #********************************************************************************************
        if (K==4){
            Mj <- cbind( 1 , sapply( 1:4 , FUN = function(jj){ 1*(Aj[,jj]==1) } ) )
               Mj.lab <- c( "0" , 1:4 )
            for (kk in 2:4){
                g2 <- utils::combn( 4 , kk )
                if (kk==4){
                        g2 <- matrix( g2 , nrow=length(g2) , ncol=1 )
                            }
                    Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                    Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
                            }
            if (rule == "DINA"){
                M <- 2^4
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv]
                                }
            if (rule == "DINO"){
                M <- 2^4
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj[,2] <- 1 ; Mj[1,2] <- 0
                Mj.lab <- Mj.lab[ selv]
                Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
                                }

            if (rule == "ACDM" | rule == "GDINA1"){
                selv <- 1:5
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv ]
                                }
            if (rule == "GDINA2"){
                selv <- 1:( 1 + 4 + 6 )
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv ]
                                }
                   }
        #***************************************
        if (K==5){
            Mj <- cbind( 1 , sapply( 1:5 , FUN = function(jj){ 1*(Aj[,jj]==1) } ) )
               Mj.lab <- c( "0" , 1:5 )
            for (kk in 2:5){
                g2 <- utils::combn( 5 , kk )
                if (kk==5){
                        g2 <- matrix( g2 , nrow=length(g2) , ncol=1 )
                            }
                    Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                    Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
                            }
            if (rule == "DINA"){
                M <- 2^5
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv]
                                }
            if (rule == "DINO"){
                M <- 2^5
                selv <- c(1,M)
                Mj <- Mj[,selv]
                Mj[,2] <- 1 ; Mj[1,2] <- 0
                Mj.lab <- Mj.lab[ selv]
                Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
                                }

            if (rule == "ACDM" | rule == "GDINA1"){
                selv <- 1:6
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv ]
                                }
            if (rule == "GDINA2"){
                selv <- 1:( 1 + 5 + 10 )
                Mj <- Mj[,selv]
                Mj.lab <- Mj.lab[  selv ]
                                }

                   }
            #*******************
            if ( (K > 5 ) | ( rule == "SISM" ) ){
                res <- create_Mj_helper( K=K , Aj = Aj , rule = rule, bugs=bugs,
                                q_jj=q_jj )
                Mj <- res$Mj
                Mj.lab <- res$Mj.lab
                        }
            res <- list( Mj , Mj.lab )
           return(res)
                     }
#####################################################################

create_Mj_helper <- function( K , Aj , rule, bugs=NULL, q_jj = NULL )
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
