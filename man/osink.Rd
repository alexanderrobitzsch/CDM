%% File Name: osink.Rd
%% File Version: 0.08

\name{osink}
\alias{osink}
\alias{csink}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{
Opens and Closes a \code{sink} Connection
}


\description{
Opens and closes a \code{sink} connection.
}

\usage{
osink(file, suffix, append = FALSE)

csink(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
File name. No \code{sink} is done if it has the value \code{NULL}.
}
  \item{suffix}{
  Suffix which should be put next to the file name
}
\item{append}{Optional logical indicating whether console output should
be appended to an already existing file. See argument \code{append} in
\code{\link[base:sink]{base::sink}}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


%\value{
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
\code{\link[base:sink]{base::sink}}
}

\examples{
## The function 'osink' is currently defined as
function (file, suffix){
    if (!is.null(file)) {
        base::sink(paste0(file, suffix), split = TRUE)
       }
  }

## The function 'csink' is currently defined as
function (file){
    if (!is.null(file)) {
        base::sink()
        }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
