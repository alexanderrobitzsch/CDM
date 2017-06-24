\name{personfit.appropriateness}
\alias{personfit.appropriateness}
\alias{summary.personfit.appropriateness}
\alias{plot.personfit.appropriateness}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Appropriateness Statistic for Person Fit Assessment
}

\description{
This function computes the person fit appropriateness statistics
(Levine & Drasgow, 1988) as proposed for cognitive diagnostic
models by Liu, Douglas and Henson (2009). The appropriateness statistic
assesses spuriously high scorers (\code{attr.type=1}) and
spuriously low scorers (\code{attr.type=0}).
}

\usage{
personfit.appropriateness(data, probs, skillclassprobs, h = 0.001, eps = 1e-10, 
    maxiter=30 , conv = 1e-05, max.increment = 0.1, progress = TRUE)
    
\method{summary}{personfit.appropriateness}(object, digits = 3 ,  \dots)

\method{plot}{personfit.appropriateness}(x, cexpch=.65 ,  \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame of dichotomous item responses
}
  \item{probs}{
Probabilities evaluated at skill space (abilities \eqn{\theta})
}
  \item{skillclassprobs}{
Probabilities of skill classes
}
  \item{h}{
Numerical differentiation parameter
}
  \item{eps}{
Constant which is added to probabilities avoiding zero probability
}
\item{maxiter}{Maximum number of iterations}
  \item{conv}{
Convergence criterion
}
  \item{max.increment}{
Maximum increment in iteration
}
  \item{progress}{
Optional logical indicating whether iteration progress should
be displayed.
}
\item{object}{Object of class \code{personfit.appropriateness}}
\item{digits}{Number of digits for rounding}
\item{x}{Object of class \code{personfit.appropriateness}}
\item{cexpch}{Point size in plot}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries
  \item{summary}{Summaries of person fit statistic}
  \item{personfit.appr.type1}{Statistic for spuriously high scorers
    (\code{appr.type=1}) evaluated for every person.}  
  \item{personfit.appr.type0}{Statistic for spuriously low scorers
    (\code{appr.type=0}) evaluated for every person.}
}

\references{
Levine, M. V., & Drasgow, F. (1988). Optimal appropriateness measurement. 
\emph{Psychometrika}, \bold{53}, 161-176.

Liu, Y., Douglas, J. A., & Henson, R. A. (2009). Testing person fit in cognitive 
diagnosis. \emph{Applied Psychological Measurement}, \bold{33(8)}, 579-598.
}


%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
#############################################################################
# EXAMPLE 1: DINA model data.ecpe
#############################################################################

data(data.ecpe)

# fit DINA model 
mod1 <- CDM::din( data.ecpe$data[,-1] , q.matrix= data.ecpe$q.matrix )
summary(mod1)

# person fit appropriateness statistic
data <- mod1$data
probs <- mod1$pjk
skillclassprobs <- mod1$attribute.patt[,1]
res <- CDM::personfit.appropriateness( data , probs , skillclassprobs , maxiter=8)
                 # only few iterations
summary(res)
plot(res)

\dontrun{
#############################################################################
# EXAMPLE 2: Person fit 2PL model
#############################################################################

library(sirt)
data( data.read , package="sirt")
dat <- data.read
I <- ncol(dat)

# fit 2PL model
mod1 <- sirt::rasch.mml2( dat , est.a= 1:I)
# person fit statistic
data <- mod1$dat
probs0 <- t(mod1$pjk)
probs <- array( 0 , dim=c( ncol(dat) , 2 , dim(probs0)[2] ) )
probs[,2,] <- probs0
probs[,1,] <- 1 - probs0
skillclassprobs <- mod1$trait.distr$pi.k
res <- CDM::personfit.appropriateness( data , probs , skillclassprobs )
summary(res)
plot(res)          
}
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Person fit}
\keyword{summary}
\keyword{plot}
