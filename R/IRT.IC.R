## File Name: IRT.IC.R
## File Version: 0.121


#--- information criteria
IRT.IC <- function( object )
{
    ll <- logLik(object)
    res <- c( ll, -2*ll, attr(ll, "df"), attr(ll,"nobs" ) )
    names(res) <- c("loglike", "Deviance", "Npars", "Nobs" )
    p <- Npars <- res["Npars"]
    n <- res["Nobs"]
    res["AIC"]  <- -2*ll + 2*Npars
    res["BIC"]  <- -2*ll + log(n)*p
    res["AIC3"] <- -2*ll + 3*Npars
    res["AICc"] <- -2*ll + 2*p + 2*p*(p+1)/(n-p-1)
    res["CAIC"] <- -2*ll + (log(n)+1)*p
    #- add GHP if included
    if ( !is.null(object$ic$GHP)){
        res["GHP"] <- object$ic$GHP
    }
    return(res)
}

