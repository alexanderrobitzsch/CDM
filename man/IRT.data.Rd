%% File Name: IRT.data.Rd
%% File Version: 0.11

\name{IRT.data}
\alias{IRT.data}
\alias{IRT.data.din}
\alias{IRT.data.gdina}
\alias{IRT.data.mcdina}
\alias{IRT.data.gdm}
\alias{IRT.data.slca}
\alias{IRT.data.reglca}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S3 Method for Extracting Used Item Response Dataset
}

\description{
This S3 method extracts the used dataset with item responses.
}

\usage{
IRT.data(object, ...)

\method{IRT.data}{din}(object, \dots)

\method{IRT.data}{gdina}(object, \dots)

\method{IRT.data}{gdm}(object, \dots)

\method{IRT.data}{mcdina}(object, \dots)

\method{IRT.data}{reglca}(object, \dots)

\method{IRT.data}{slca}(object, \dots)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{\link{din}}, \code{\link{gdina}},
\code{\link{mcdina}}, \code{\link{gdm}}, \code{\link{slca}},
\code{\link{reglca}}.
}
  \item{\dots}{
More arguments to be passed.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A matrix (or data frame) with item responses and group identifier and
weights vector as attributes.
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
\dontrun{
#############################################################################
# EXAMPLE 1: Several models for sim.dina data
#############################################################################

dat <- CDM::sim.dina
q.matrix <- CDM::sim.qmatrix

#--- Model 1: GDINA model
mod1 <- CDM::gdina( data = dat, q.matrix = q.matrix)
summary(mod1)
dmod1 <- CDM::IRT.data(mod1)
str(dmod1)

#--- Model 2: DINA model
mod2 <- CDM::din( data = dat, q.matrix = q.matrix)
summary(mod2)
dmod2 <- CDM::IRT.data(mod2)

#--- Model 3: Rasch model with gdm function
mod3 <- CDM::gdm( data = dat, irtmodel="1PL" , theta.k=seq(-4,4,length=11) ,
                centered.latent=TRUE )
summary(mod3)
dmod3 <- CDM::IRT.data(mod3)

#--- Model 4: Latent class model with two classes

dat <- sim.dina
I <- ncol(dat)

# define design matrices
TP <- 2     # two classes
# The idea is that latent classes refer to two different "dimensions".
# Items load on latent class indicators 1 and 2, see below.
Xdes <- array(0 , dim=c(I,2,2,2*I) )
items <- colnames(dat)
dimnames(Xdes)[[4]] <- c(paste0( colnames(dat) , "Class" , 1),
          paste0( colnames(dat) , "Class" , 2) )
    # items, categories , classes , parameters
# probabilities for correct solution
for (ii in 1:I){
    Xdes[ ii , 2 , 1 , ii ] <- 1    # probabilities class 1
    Xdes[ ii , 2 , 2 , ii+I ] <- 1  # probabilities class 2
}
# estimate model
mod4 <- CDM::slca( dat , Xdes=Xdes)            
summary(mod4)
dmod4 <- CDM::IRT.data(mod4)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{Expected counts}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
