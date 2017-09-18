%% File Name: summary_sink.Rd
%% File Version: 0.04
%% File Last Change: 2017-01-31 14:08:32

\name{summary_sink}
\alias{summary_sink}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prints \code{summary} and \code{sink} Output in a File
}
\description{
Prints \code{summary} and \code{sink} output in a File
}
\usage{
summary_sink( object , file , append=FALSE , ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object for which a \code{summary} method is defined
}
  \item{file}{
File name
}
\item{append}{Optional logical indicating whether console output should
be appended to an already existing file. See argument \code{append} in 
\code{\link[base:sink]{base::sink}}.}
\item{\dots}{Further arguments passed to \code{summary}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

\seealso{
\code{\link[base:sink]{base::sink}},
\code{\link[base:summary]{base::summary}}
}


\examples{
#############################################################################
# EXAMPLE 1: summary_sink example for lm function
#############################################################################	

#--- simulate some data
set.seed(997)
N <- 200
x <- stats::rnorm( N )
y <- .4 * x + stats::rnorm(N , sd =.5 )

#--- fit a linear model and sink summary into a file
mod1 <- stats::lm( y ~ x )
summary_sink(mod1 , file = "my_model")

#--- fit a second model and append it to file
mod2 <- stats::lm( y ~ x + I(x^2) )
summary_sink(mod2 , file = "my_model" , append = TRUE )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
