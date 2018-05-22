%% File Name: IRT.repDesign.Rd
%% File Version: 0.21

\name{IRT.repDesign}
\alias{IRT.repDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generation of a Replicate Design for \code{IRT.jackknife}
}

\description{
This function generates a Jackknife replicate design which is
necessary to use the \code{IRT.jackknife} function. The function
is a wrapper to \code{BIFIE.data.jack} in the \pkg{BIFIEsurvey} package.
}

\usage{
IRT.repDesign(data, wgt=NULL, jktype="JK_TIMSS", jkzone=NULL, jkrep=NULL,
   jkfac=NULL, fayfac=1, wgtrep="W_FSTR", ngr=100, Nboot=200, seed=.Random.seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset which must contain weights and item responses
}
  \item{wgt}{
Vector with sample weights
}
  \item{jktype}{
Type of jackknife procedure for creating the BIFIE.data object.
\code{jktype="JK_TIMSS"} refers to TIMSS/PIRLS datasets.
The type \code{"JK_GROUP"} creates jackknife weights based on a user defined grouping,
the type \code{"JK_RANDOM"} creates random groups.
The number of random groups can be defined in ngr.
The argument \code{type="RW_PISA"} extracts the replicated design with
balanced repeated replicate weights from PISA datasets into objects of
class \code{IRT.repDesign}.
Bootstrap samples can be obtained by \code{type="BOOT"}.
}
\item{jkzone}{
Variable name for jackknife zones.
If \code{jktype="JK_TIMSS"}, then \code{jkzone="JKZONE"}. However,
this default can be overwritten.
}
  \item{jkrep}{
Variable name containing Jackknife replicates
}
  \item{jkfac}{
Factor for multiplying jackknife replicate weights.
If \code{jktype="JK_TIMSS"}, then \code{jkfac=2}.
}
  \item{fayfac}{
Fay factor. For Jackknife, the default is 1. For a Bootstrap with
\eqn{R} samples with replacement, the Fay factor is \eqn{1/R}.
}
  \item{wgtrep}{
Already available replicate design
}
  \item{ngr}{
Number of groups
}
\item{Nboot}{Number of bootstrap samples}
  \item{seed}{
Random seed
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with following entries
 \item{wgt}{Vector with weights}
  \item{wgtrep}{Matrix containing the replicate design}
  \item{fayfac}{Fay factor needed for Jackknife calculations}
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
See \code{\link{IRT.jackknife}} for further examples.

See the \code{BIFIE.data.jack} function in the \pkg{BIFIEsurvey} package.
}


\examples{
\dontrun{
# load the BIFIEsurvey package
library(BIFIEsurvey)

#############################################################################
# EXAMPLE 1: Design with Jackknife replicate weights in TIMSS
#############################################################################

data(data.timss11.G4.AUT, package="CDM")
dat <- CDM::data.timss11.G4.AUT$data
# generate design
rdes <- CDM::IRT.repDesign( data=dat,  wgt="TOTWGT", jktype="JK_TIMSS",
             jkzone="JKCZONE", jkrep="JKCREP" )
str(rdes)

#############################################################################
# EXAMPLE 2: Bootstrap resampling
#############################################################################

data(sim.qmatrix, package="CDM")
q.matrix <- CDM::sim.qmatrix

# simulate data according to the DINA model
dat <- CDM::sim.din(N=2000, q.matrix=q.matrix )$dat

# bootstrap with 300 random samples
rdes <- CDM::IRT.repDesign( data=dat, jktype="BOOT", Nboot=300 )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Replication weights}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
