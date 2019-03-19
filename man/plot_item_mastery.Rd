%% File Name: plot_item_mastery.Rd
%% File Version: 0.07

\name{plot_item_mastery}
\alias{plot_item_mastery}
\alias{plot_item_mastery.din}
\alias{plot_item_mastery.gdina}


%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S3 Methods for Plotting Item Probabilities
}

\description{
This S3 method plots item probabilities for non-masters and masters of an item.
}

\usage{
plot_item_mastery(object, pch=c(16,17), lty=c(1,2), \dots)

\method{plot_item_mastery}{din}(object, pch=c(16,17), lty=c(1,2), \dots)

\method{plot_item_mastery}{gdina}(object, pch=c(16,17), lty=c(1,2), \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{\link{din}} or \code{\link{gdina}}
}
\item{pch}{Point symbols for both groups}
\item{lty}{Line symbols for both groups}
  \item{\dots}{
More arguments to be passed.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Plot
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
Plot functions for item response curves: \code{\link{IRT.irfprobPlot}}.
}
%%% TAM, sirt and mirt links

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Plot item mastery
#############################################################################

data(sim.dina)
data(sim.qmatrix)

#* estimate DINA Model
mod1 <- CDM::din(sim.dina, q.matrix=sim.qmatrix, rule="DINA")
#* estimate GDINA model
mod2 <- CDM::gdina(sim.dina, q.matrix=sim.qmatrix)

#* plots
plot_item_mastery(mod1)
plot_item_mastery(mod2)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Individual likelihood}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
