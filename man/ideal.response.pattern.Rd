%% File Name: ideal.response.pattern.Rd
%% File Version: 0.08

\name{ideal.response.pattern}
\alias{ideal.response.pattern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ideal Response Pattern
}
\description{
This function computes the ideal response pattern which is the latent
item response \eqn{\eta_{lj} = \prod_{k=1}^K \alpha_{lk}} for a person
with skill profile \eqn{l} at item \eqn{j}.
}

\usage{
ideal.response.pattern(q.matrix, skillspace = NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q.matrix}{
The Q-matrix
}
  \item{skillspace}{
An optional skill space matrix. If it is not provided, then all skill
classes are used for creating an ideal response pattern.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
% }


\value{
A list with following entries

\item{idealresp}{A matrix with ideal response patterns}
\item{skillspace}{Used skill space}
}


%\references{
%% ~put references to the literature/web site here ~
%}
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
# EXAMPLE 1: Ideal response pattern sim.qmatrix
#############################################################################

data(sim.qmatrix, package="CDM")

q.matrix <- sim.qmatrix
CDM::ideal.response.pattern( q.matrix )

# compute ideal responses for a reduced skill space
skillspace <- matrix( c( 0,1,0 ,
                         1,1,0 ) , 2 ,3 , byrow=TRUE )
CDM::ideal.response.pattern( q.matrix , skillspace=skillspace)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Ideal response pattern}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
