%% File Name: IRT.marginal_posterior.Rd
%% File Version: 0.07

\name{IRT.marginal_posterior}
\alias{IRT.marginal_posterior}
\alias{IRT.marginal_posterior.din}
\alias{IRT.marginal_posterior.gdina}
\alias{IRT.marginal_posterior.mcdina}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S3 Method for Computation of Marginal Posterior Distribution
}

\description{
Computes marginal posterior distributions for fitted models in the
\pkg{CDM} package.
}


\usage{
IRT.marginal_posterior(object, dim, remove_zeroprobs=TRUE, ...)

\method{IRT.marginal_posterior}{din}(object, dim, remove_zeroprobs=TRUE, ...)
\method{IRT.marginal_posterior}{gdina}(object, dim, remove_zeroprobs=TRUE, ...)
\method{IRT.marginal_posterior}{mcdina}(object, dim, remove_zeroprobs=TRUE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{\link{din}}, \code{\link{gdina}}, \code{\link{mcdina}}
}
\item{dim}{
Numeric or character vector indicating dimensions of posterior distribution
which should be marginalized
}
  \item{remove_zeroprobs}{
Logical indicating whether classes with zero probabilities should
be removed
}
  \item{\dots}{
Further arguments to be passed
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with entries
\item{marg_post}{Marginal posterior distribution}
\item{map}{MAP estimate (individual classification)}
\item{theta}{Skill classes}
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

\seealso{
\code{\link{IRT.posterior}}
}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Dataset with three hierarchical skills
#############################################################################

# simulated data with hierarchical skills:
# skill A with 4 levels, skill B with 2 levels and skill C with 3 levels

data(data.cdm10, package="CDM"")
dat <- data.cdm10$data
Q <- data.cdm10$q.matrix
print(Q)

# define hierarchical skill structure
B <- "A1 > A2 > A3
      C1 > C2"
skill_space <- CDM::skillspace.hierarchy(B=B, skill.names=colnames(Q))
zeroprob.skillclasses <- skill_space$zeroprob.skillclasses

# estimate DINA model
mod1 <- CDM::gdina( dat, q.matrix=Q, zeroprob.skillclasses=zeroprob.skillclasses, rule="DINA")
summary(mod1)

# classification for skill A
res <- CDM::IRT.marginal_posterior(object=mod1, dim=c("A1","A2","A3") )
table(res$map)

# classification for skill B
res <- CDM::IRT.marginal_posterior(object=mod1, dim=c("B") )
table(res$map)

# classification for skill C
res <- CDM::IRT.marginal_posterior(object=mod1, dim=c("C1","C2") )
table(res$map)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
