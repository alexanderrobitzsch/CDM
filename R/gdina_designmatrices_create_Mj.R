## File Name: gdina_designmatrices_create_Mj.R
## File Version: 0.07



#***************************************************************
# design matrix Mj
gdina_designmatrices_create_Mj <- function( Aj , rule = "GDINA" , q_jj = NULL, bugs=NULL)
{
    K <- ncol(Aj)
    Mj <- NULL
    Mj.lab <- NULL
    #***********************************************
    if (K==1){
        Mj <- matrix( c( 1,0, 1 ,1 ) , byrow=TRUE , ncol=2^1 )
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
        res <- gdina_designmatrices_create_Mj_general( K=K , Aj = Aj , rule = rule, bugs=bugs,
                            q_jj=q_jj )
        Mj <- res$Mj
        Mj.lab <- res$Mj.lab
    }
    res <- list( Mj , Mj.lab )
    return(res)
}
#####################################################################


.create.Mj <- gdina_designmatrices_create_Mj
