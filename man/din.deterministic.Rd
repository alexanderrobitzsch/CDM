%% File Name: din.deterministic.Rd
%% File Version: 0.16
%% File Last Change: 2017-01-31 14:08:30

\name{din.deterministic}
\alias{din.deterministic}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Deterministic Classification and Joint Maximum Likelihood Estimation
of the Mixed DINA/DINO Model
}

\description{
This function allows the estimation of the mixed DINA/DINO model by
joint maximum likelihood and a deterministic classification based
on ideal latent responses.
}

\usage{
din.deterministic(dat, q.matrix, rule="DINA" , method = "JML", conv = 0.001, 
    maxiter = 300, increment.factor = 1.05, progress = TRUE)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{dat}{
Data frame of dichotomous item responses
}
  \item{q.matrix}{
Q-matrix with binary entries (see \code{\link{din}}).
}
\item{rule}{The condensation rule (see \code{\link{din}}).
}
\item{method}{
Estimation method. The default is joint maximum likelihood estimation
(\code{JML}). Other options include an adaptive estimation of guessing and 
slipping parameters (\code{adaptive}) while using these estimated parameters 
as weights in the individual deviation function and classification based on
the Hamming distance (\code{hamming}) and the weighted Hamming
distance (\code{weighted.hamming}) (see Chiu & Douglas, 2013).
}
  \item{conv}{
Convergence criterion for guessing and slipping parameters
}
  \item{maxiter}{
Maximum number of iterations
}
  \item{increment.factor}{
A numeric value of at least one which could help to improve convergence 
behavior and decreases parameter increments in every iteration. This option is
disabled by setting this argument to 1.
}
\item{progress}{An optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  } 
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with following entries
\item{attr.est}{Estimated attribute patterns}
\item{criterion}{Criterion of the classification function. 
    For joint maximum likelihood it is the deviance.}
\item{guess}{Estimated guessing parameters}
\item{slip}{Estimated slipping parameters}
\item{prederror}{Average individual prediction error}
\item{q.matrix}{Used Q-matrix}
\item{dat}{Used data frame}
}

\references{
Chiu, C. Y., & Douglas, J. (2013). A nonparametric approach to 
cognitive diagnosis by proximity to ideal response patterns. 
\emph{Journal of Classification}, \bold{30}, 225-250.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For estimating the mixed DINA/DINO model using marginal maximum
likelihood estimation see \code{\link{din}}.

See also the \code{NPCD::JMLE} function in the \pkg{NPCD} package for 
joint maximum likelihood estimation of the DINA or the DINO model.
}

\examples{
#############################################################################
# EXAMPLE 1: 13 items and 3 attributes
#############################################################################

set.seed(679)
N <- 3000
# specify true Q-matrix
q.matrix <- matrix( 0 , 13 , 3 )
q.matrix[1:3,1] <- 1
q.matrix[4:6,2] <- 1
q.matrix[7:9,3] <- 1
q.matrix[10,] <- c(1,1,0)
q.matrix[11,] <- c(1,0,1)
q.matrix[12,] <- c(0,1,1)
q.matrix[13,] <- c(1,1,1)
q.matrix <- rbind( q.matrix , q.matrix )
colnames(q.matrix) <- paste0("Attr",1:ncol(q.matrix))

# simulate data according to the DINA model
dat <- sim.din( N=N , q.matrix)$dat

# Joint maximum likelihood estimation (the default: method="JML")
res1 <- CDM::din.deterministic( dat , q.matrix )

# Adaptive estimation of guessing and slipping parameters
res <- CDM::din.deterministic( dat , q.matrix , method="adaptive" )

# Classification using Hamming distance
res <- CDM::din.deterministic( dat , q.matrix , method="hamming" )

# Classification using weighted Hamming distance
res <- CDM::din.deterministic( dat , q.matrix , method="weighted.hamming" )

\dontrun{
#********* load NPCD library for JML estimation
miceadds::library_install("NPCD")
# choose a smaller dataset for reasons of computation time

# DINA model
res <- NPCD::JMLE( Y=dat[1:100,] , Q=q.matrix , model="DINA" )
as.data.frame(res$par.est )   # item parameters
res$alpha.est                 # skill classifications

# RRUM model
res <- NPCD::JMLE( Y=dat[1:100,] , Q=q.matrix , model="RRUM" )
as.data.frame(res$par.est )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Deterministic classification}
\keyword{Cluster analysis}
\keyword{Joint maximum likelihood}% __ONLY ONE__ keyword per line
