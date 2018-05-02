## File Name: gdm_data_prep.R
## File Version: 0.05


############################################
# gdm data preparation
gdm_data_prep <- function( dat , data , weights , group )
{
    I <- ncol(dat)
    N <- nrow(dat)
    data1 <- data
    data1[ is.na(data1) ] <- 9
    cat("************************************************************\n")
    cat("Data preparation\n")
    cat("Number of rows in data =" , nrow(data1) , "\n") ; utils::flush.console()
    item.patt.subj <- data1[,1]
    for ( ii in 2:I){
        item.patt.subj <- paste( item.patt.subj  , data1[,ii] , sep="")
    }
    #****
    # arrange groups
    if ( is.null(group)){
        G <- 1
        group <- rep(1,N)
    } else {
        gr2 <- unique( sort(paste( group ) ))
        G <- length(gr2)
        group <- match( group , gr2 )
    }
    # calculate frequency of each item response pattern in case of one group
    if (G==1){
        if ( is.null(weights) ){ weights <- rep(1,N) }
        a2 <- rowsum( weights , item.patt.subj)
        item.patt <- a2[,1]
        # define data frame 'item.patt' with item response pattern and its frequency (weight)
        item.patt <- cbind( "pattern" = names(item.patt),
                "freq" = as.numeric(as.vector( item.patt ) ) )
        weights <- as.numeric(paste(item.patt[,"freq"]))
    }
    #***
    # multiple group case
    if ( is.null(weights) ){
            weights <- rep(1,N)
    }
    if (G>1){
        for (gg in 1:G){
            ind.gg <- which( group == gg )
            a2 <- rowsum( weights[ind.gg] , item.patt.subj[ind.gg] )
            a2 <- data.frame( "pattern" = rownames(a2) , a2[,1] )
            colnames(a2)[2] <- paste0("freq.Gr" , gg)
            rownames(a2) <- NULL
            if (gg == 1){ item.patt <- a2 }
            if (gg > 1){
                item.patt <- merge( item.patt , a2 , by ="pattern" , all=TRUE )
            }
            item.patt[ is.na(item.patt) ] <- 0
        }
        weights <- item.patt[,-1]
    }
    #***
    # reconstruct data
    N <- nrow(item.patt)
    dat <- matrix(NA , N , I )
    for (ii in 1:I){
        dat[,ii ] <- as.numeric( substring( item.patt[,"pattern"] , ii,ii) )
    }
    dat.resp <- 1-(dat==9)
    data <- dat
    dat[ dat.resp==0] <- 0
    cat("Number of response patterns =" , nrow(dat) , "\n")
    utils::flush.console()
    res <- list( weights=weights , dat = dat , dat.resp=dat.resp ,
                    data=data , item.patt = item.patt )
    return(res)
}
####################################################

.gdm.data.prep <- gdm_data_prep
