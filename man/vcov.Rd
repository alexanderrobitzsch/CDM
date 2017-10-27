%% File Name: vcov.Rd
%% File Version: 0.27

\name{vcov}
\alias{vcov.din}
\alias{confint.din}
\alias{IRT.se}
\alias{IRT.se.din}

\title{Asymptotic Covariance Matrix, Standard Errors and Confidence Intervals}

\description{
  Computes the asymptotic covariance matrix for 
  \code{din} objects. The covariance matrix is computed using the
  empirical cross-product approach (see Paek & Cai, 2014).
  
  In addition, an S3 method \code{IRT.se} is defined which produces 
  an extended output including \code{vcov} and \code{confint}.
}


\usage{
\method{vcov}{din}(object, extended=FALSE, infomat=FALSE ,ind.item.skillprobs=TRUE, 
       ind.item= FALSE, diagcov = FALSE , h=.001 ,\dots)
       
\method{confint}{din}(object, parm , level=.95 , extended=FALSE, 
      ind.item.skillprobs=TRUE, ind.item= FALSE, diagcov = FALSE , h=.001 , \dots ) 

IRT.se(object, \dots)

\method{IRT.se}{din}( object , extended = FALSE , parm=NULL , level = .95 , 
      infomat=FALSE , ind.item.skillprobs = TRUE , ind.item= FALSE , 
      diagcov = FALSE , h=.001 , ... )
}

\arguments{
  \item{object}{An object inheriting from class \code{din}.
  			}
\item{extended}{An optional logical indicating whether the covariance
	matrix should be calculated for an extended set of parameters
	(estimated and derived parameters).
		}
\item{infomat}{An optional logical indicating whether the
	information matrix instead of the covariance matrix should be
	the output.}
\item{ind.item.skillprobs}{Optional logical indicating whether the covariance
between item parameters and skill class probabilities are assumed
to be zero.}
\item{ind.item}{Optional logical indicating whether covariances of
item parameters between different items are zero.}
\item{diagcov}{Optional logical indicating whether all covariances
between estimated parameters are set to zero.}
\item{h}{Parameter used for numerical differiation for computing
the derivative of the log-likelihood function.}
\item{parm}{Vector of parameters. If it is missing, then for all estimated
parameters a confidence interval is calculated.}
\item{level}{Confidence level}	
  \item{\dots}{Additional arguments to be passed.}
}

%%\details{
%%%% nothing yet
%%}

\value{
   \code{coef}: A vector of paramters.
   
   \code{vcov}: A covariance matrix. The corresponding coefficients can be extracted
   as the attribute \code{coef} from this object.
     
   \code{IRT.se}: A data frame containing coefficients, standard errors
     and confidence intervals for all parameters.
	}

\references{
Paek, I., & Cai, L. (2014). A comparison of item parameter standard error 
estimation procedures for unidimensional and multidimensional item response 
theory modeling. \emph{Educational and Psychological Measurement}, \bold{74(1)},
58-76.
}

%\note{
%Because \code{coef} and \code{vcov} are defined for objects of class
%\code{din}, asymptotic confidence intervals can be computed using 
%\code{\link[stats:confint]{confint}}.
%}	

%%\author{
%% CDM authors
%%}

\seealso{
\code{\link{din}}, \code{\link{coef.din}}
}

%% \code{\link{gdina}}, \code{\link{gdm}},
%%	\code{\link{mcdina}}, \code{\link{slca}}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: DINA model sim.dina
#############################################################################	

data(sim.dina)
data(sim.qmatrix)
dat <- sim.dina

#****** Model 1: (Ordinary) DINA Model
mod1 <- CDM::din( dat , q.matr = sim.qmatrix, rule = "DINA")
# look at parameter table of the model
mod1$partable
# covariance matrix
covmat1 <- vcov(mod1 )
# extract coefficients
coef(mod1)
# extract standard errors
sqrt( diag( covmat1))
# compute confidence intervals
confint( mod1 , level=.90 )
# output table with standard errors
IRT.se( mod1 , extended=TRUE )

#****** Model 2: Constrained DINA Model

# fix some slipping parameters
constraint.slip <- cbind( c(2,3,5) , c(.15,.20,.25) )
# set some skill class probabilities to zero
zeroprob.skillclasses <- c(2,4)
# estimate model
mod2 <- CDM::din( dat , q.matr = sim.qmatrix, guess.equal=TRUE , 
     constraint.slip=constraint.slip, zeroprob.skillclasses=zeroprob.skillclasses)
# parameter table
mod2$partable
# freely estimated coefficients
coef(mod2)
# covariance matrix (estimated parameters)
vmod2a <- vcov(mod2)
sqrt( diag( vmod2a))		# standard errors
colnames( vmod2a )
names( attr( vmod2a , "coef") )	# extract coefficients

# covariance matrix (more parameters, extended=TRUE)
vmod2b <- vcov(mod2 , extended=TRUE)
sqrt( diag( vmod2b))
attr( vmod2b , "coef")
# attach standard errors to parameter table
partable2 <- mod2$partable
partable2 <- partable2[ ! duplicated( partable2$parnames ) , ]
partable2 <- data.frame( partable2 , "se" = sqrt( diag( vmod2b)) )
partable2

# confidence interval for parameter "skill1" which is not in the model
#   cannot be calculated!
confint(mod2 , parm= c( "skill1" , "all_guess" ) )
# confidence interval for only some parameters
confint(mod2 , parm=paste0("prob_skill" , 1:3 ) )

# compute only information matrix
infomod2 <- vcov(mod2 , infomat=TRUE)
}	
}

\keyword{methods}
\keyword{vcov}
\keyword{confint}
% \keyword{IRT.se}
