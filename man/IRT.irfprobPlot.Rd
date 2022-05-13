%% File Name: IRT.irfprobPlot.Rd
%% File Version: 0.181

\name{IRT.irfprobPlot}
\alias{IRT.irfprobPlot}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Plot Item Response Functions
}

\description{
This function plots item response functions for fitted
item response models for which the \code{\link{IRT.irfprob}}
method is defined.
}

\usage{
IRT.irfprobPlot( object, items=NULL, min.theta=-4, max.theta=4, cumul=FALSE,
     smooth=TRUE, ask=TRUE,  n.theta=40, package="lattice",... )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Fitted item response model for which the \code{\link{IRT.irfprob}}
method is defined
}
\item{items}{Vector of indices of selected items.}
\item{min.theta}{Minimum theta to be displayed.}
\item{max.theta}{Maximum theta to be displayed.}
\item{cumul}{Optional logical indicating whether cumulated
    item response functions \eqn{P( X \ge k | \theta )}
should be displayed.}
\item{smooth}{Optional logical indicating whether item response
functions should be smoothed for plotting.}
\item{ask}{Logical for asking for a new plot.}
\item{n.theta}{Number of theta points if \code{smooth=TRUE} is chosen.}
\item{package}{String indicating which package should be used for plotting
    the item response curves. Options are \code{"lattice"}
        or \code{"graphics"}.}
  \item{\dots}{
More arguments to be passed for the plot in \pkg{lattice}.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


%\value{
%An array with item response probabilities (items \eqn{\times}
%}


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
%For extracting the individual likelihood or posterior see
%\code{\link{IRT.likelihood}} or \code{\link{IRT.posterior}}.
%}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Plot item response functions from a unidimensional model
#############################################################################

data(data.Students, package="CDM")

dat <- data.Students
resp <- dat[, paste0("sc",1:4) ]
resp[ paste(resp[,1])==3,1] <-  2
psych::describe(resp)

#--- Model 1: PCM in CDM::gdm
theta.k <- seq( -5, 5, len=21 )
mod1 <- CDM::gdm( dat=resp, irtmodel="1PL", theta.k=theta.k, skillspace="normal",
           centered.latent=TRUE)
summary(mod1)

# plot
IRT.irfprobPlot( mod1 )
# plot in graphics package (which comes with R base version)
IRT.irfprobPlot( mod1, package="graphics")
# plot first and third item and do not smooth discretized item response
# functions in IRT.irfprob
IRT.irfprobPlot( mod1, items=c(1,3), smooth=FALSE )
# cumulated IRF
IRT.irfprobPlot( mod1, cumul=TRUE )

#############################################################################
# EXAMPLE 2: Fitted mutidimensional model with gdm
#############################################################################

dat <- CDM::data.fraction2$data
Qmatrix <- CDM::data.fraction2$q.matrix3

# Model 1: 3-dimensional Rasch Model (normal distribution)
theta.k <- seq( -4, 4, len=11 )   # discretized ability
mod1 <- CDM::gdm( dat, irtmodel="1PL", theta.k=theta.k, Qmatrix=Qmatrix,
              centered.latent=TRUE, maxiter=10 )
summary(mod1)

# unsmoothed curves
IRT.irfprobPlot(mod1, smooth=FALSE)
# smoothed curves
IRT.irfprobPlot(mod1)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Individual likelihood}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
