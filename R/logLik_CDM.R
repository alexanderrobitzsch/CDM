## File Name: logLik_CDM.R
## File Version: 0.11
#########################################
# Log likelihood functions
#########################################
# din class
logLik.din <- function (object, ...)
{
	# extract log-likelihood
	out <- object$loglike
	# number of parameters
	attr(out, "df") <- sum( object$Npars )
	# extract number of observations
	attr(out, "nobs") <- object$I
	class(out) <- "logLik"
	return(out)
}
#########################################
# gdina class
logLik.gdina <- function (object, ...)
{
	out <- object$loglike
	attr(out, "df") <- sum( object$Npars )
	attr(out, "nobs") <- sum(object$N)
	class(out) <- "logLik"
	return(out)
}
###########################################
# gdm class 
logLik.gdm <- function (object, ...)
{
	out <- object$loglike
	attr(out, "df") <- sum( object$Npars )
	attr(out, "nobs") <- sum(object$N)
	class(out) <- "logLik"
	return(out)
}
#############################################
#########################################
# mcdina class
logLik.mcdina <- function (object, ...)
{
	out <- object$loglike
	attr(out, "df") <- sum( object$Npars )
	attr(out, "nobs") <- sum(object$I)
	class(out) <- "logLik"
	return(out)
}
#############################################
#########################################
# slca class
logLik.slca <- function (object, ...)
{
	out <- object$loglike
	attr(out, "df") <- sum( object$Npars )
	attr(out, "nobs") <- sum(object$N)
	class(out) <- "logLik"
	return(out)
}
#############################################
#########################################
# reglca class
logLik.reglca <- function (object, ...)
{
	out <- object$loglike
	attr(out, "df") <- sum(object$Npars)
	attr(out, "nobs") <- sum(object$N)
	class(out) <- "logLik"
	return(out)
}
