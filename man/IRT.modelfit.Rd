%% File Name: IRT.modelfit.Rd
%% File Version: 0.234

\name{IRT.modelfit}
\alias{IRT.modelfit}
\alias{IRT.modelfit.din}
\alias{IRT.modelfit.gdina}
\alias{IRT.modelfit.gdm}
\alias{summary.IRT.modelfit.din}
\alias{summary.IRT.modelfit.gdina}
\alias{summary.IRT.modelfit.gdm}


%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S3 Methods for Assessing Model Fit
}

\description{
This S3 method assesses global (absolute) model fit using
the methods described in \code{\link{modelfit.cor.din}}.
}

\usage{
IRT.modelfit(object, \dots)

\method{IRT.modelfit}{din}(object, \dots)
\method{IRT.modelfit}{gdina}(object, \dots)
\method{IRT.modelfit}{gdm}(object, \dots)

\method{summary}{IRT.modelfit.din}(object, \dots)
\method{summary}{IRT.modelfit.gdina}(object, \dots)
\method{summary}{IRT.modelfit.gdm}(object, \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{\link{din}}, \code{\link{gdina}} or \code{\link{gdm}}.
}
  \item{\dots}{
More arguments to be passed.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
See output of \code{\link{modelfit.cor.din}}.
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
For extracting the individual likelihood or posterior see
\code{\link{IRT.likelihood}} or \code{\link{IRT.posterior}}.

The model fit of objects of class \code{\link{gdm}} can be obtained
by using the
\code{TAM::tam.modelfit.IRT} function in the \bold{TAM} package.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Absolute model fit
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

#*** Model 1: DINA model for DINA simulated data
mod1 <- CDM::din( sim.dina, q.matrix=sim.qmatrix, rule="DINA" )
fmod1 <- CDM::IRT.modelfit( mod1 )
summary(fmod1)
  ##  Test of Global Model Fit
  ##         type value     p
  ##  1   max(X2) 8.728 0.113
  ##  2 abs(fcor) 0.143 0.080
  ##
  ##  Fit Statistics
  ##                    est
  ##  MADcor          0.030
  ##  SRMSR           0.040
  ##  100*MADRESIDCOV 0.671
  ##  MADQ3           0.062
  ##  MADaQ3          0.059

#*** Model 2: GDINA model
mod2 <- CDM::gdina( sim.dina, q.matrix=sim.qmatrix, rule="GDINA" )
fmod2 <- CDM::IRT.modelfit( mod2 )
summary(fmod2)
  ##  Test of Global Model Fit
  ##         type value p
  ##  1   max(X2) 2.397 1
  ##  2 abs(fcor) 0.078 1
  ##
  ##  Fit Statistics
  ##                    est
  ##  MADcor          0.023
  ##  SRMSR           0.030
  ##  100*MADRESIDCOV 0.515
  ##  MADQ3           0.075
  ##  MADaQ3          0.071
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Individual likelihood}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
