%% File Name: entropy.lca.Rd
%% File Version: 0.18

\name{entropy.lca}
\alias{entropy.lca}
\alias{summary.entropy.lca}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Test-specific and Item-specific Entropy for Latent Class Models 
}

\description{
Computes test-specific and item-specific entropies as test-diagnostic
criteria of cognitive diagnostic models (Asparouhov & Muthen, 2014).
}

\usage{
entropy.lca(object)

\method{summary}{entropy.lca}(object, digits = 2 ,  \dots)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{object}{
Object of class \code{\link{din}}, \code{\link{gdina}} or \code{\link{mcdina}}. 
For the \code{summary} method, it is the result of \code{entropy.lca}.
}
\item{digits}{Number of digits to round}
\item{\dots}{Further arguments to be passed}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with the data frame \code{entropy} as an entry. 
}


\references{
Asparouhov, T. & Muthen, B. (2014). \emph{Variable-specific entropy
contribution}. Technical Appendix. \url{http://www.statmodel.com/7_3_papers.shtml}
}

%\author{
%Alexander Robitzsch
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{cdi.kli}} for test diagnostic indices based on the 
Kullback-Leibler information and \code{\link{cdm.est.class.accuracy}}
for calculating the classification accuracy.
}

\examples{
#############################################################################
# EXAMPLE 1: Entropy for DINA model 
#############################################################################	

# fit DINA Model
mod1 <- CDM::din( CDM::sim.dina, q.matrix=CDM::sim.qmatrix, rule = "DINA")
summary(mod1)
# compute entropy for test and items
emod1 <- CDM::entropy.lca( mod1 )
summary(emod1)	

\dontrun{
#############################################################################
# EXAMPLE 2: Entropy for polytomous GDINA model
#############################################################################

dat <- CDM::data.pgdina$dat
q.matrix <- CDM::data.pgdina$q.matrix

# pGDINA model with "DINA rule"
mod1 <- CDM::gdina( dat, q.matrix=q.matrix, rule="DINA")
summary(mod1)

# compute entropy
emod1 <- CDM::entropy.lca( mod1 )
summary(emod1)

#############################################################################
# EXAMPLE 3: Entropy for MCDINA model
#############################################################################

dat <- CDM::data.cdm02$data
q.matrix <- CDM::data.cdm02$q.matrix

# estimate model with polytomous atribute
mod1 <- CDM::mcdina( dat, q.matrix=q.matrix )
summary(mod1)
# computre entropy
emod1 <- CDM::entropy.lca( mod1 )
summary(emod1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{Entropy}
%\keyword{din}
%\keyword{gdina}
%\keyword{mcdina}
\keyword{summary}% __ONLY ONE__ keyword per line
