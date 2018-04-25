%% File Name: discrim.index.Rd
%% File Version: 0.09

\name{discrim.index}
\alias{discrim.index}
\alias{discrim.index.din}
\alias{discrim.index.gdina}
\alias{summary.discrim.index}


%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Discrimination Indices at Item and Test Level
}


\description{
Computes discrimination indices at the probability metric (Henson, DiBello & Stout, 2018).
}

\usage{
discrim.index(object, ...)

\method{discrim.index}{din}(object, \dots)

\method{discrim.index}{gdina}(object, \dots)

\method{summary}{discrim.index}(object, file = NULL, digits = 3,  \dots)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{\link{din}} or \code{\link{gdina}}.
}
\item{file}{Optional file name for a file in which the summary
output should be sunk}
\item{digits}{Number of digits for rounding}
\item{\dots}{Further arguments to be passed}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with following entries

\item{discrim_item}{Discrimination indices at item level}
\item{discrim_test}{Discrimination index at test level}
}

\references{
Henson, R., DiBello, L., & Stout, B. (2018). A generalized approach to defining item 
discrimination for DCMs. 
\emph{Measurement: Interdisciplinary Research and Perspectives, 16}(1), 18-29.
}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{cdi.kli}} for discrimination indices based on the 
Kullback-Leibler information.
}

\examples{
#############################################################################
# EXAMPLE 1: DINA and GDINA model
#############################################################################	

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

#-- fit GDINA and DINA model
mod1 <- CDM::gdina( sim.dina, q.matrix=sim.qmatrix )
mod2 <- CDM::din( sim.dina, q.matrix=sim.qmatrix )

#-- compute discrimination indices
dimod1 <- CDM::discrim.index(mod1)
dimod2 <- CDM::discrim.index(mod2)
summary(dimod1)
summary(dimod2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item discrimination}
\keyword{Cognitive diagnostic discrimination}
\keyword{summary}
