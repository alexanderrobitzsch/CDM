%% File Name: skill.cor.Rd
%% File Version: 2.06

\name{skill.cor}
\alias{skill.cor}
\alias{skill.polychor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tetrachoric or Polychoric Correlations between Attributes
}
\description{
This function takes the results of \code{din} or \code{gdina} and
computes tetrachoric or polychoric correlations between attributes (see e.g. 
Templin & Henson, 2006).
}
\usage{
# tetrachoric correlations
skill.cor(object)     

# polychoric correlations
skill.polychor(object, colindex=1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{din} or \code{gdina}
}
\item{colindex}{Index which can used for group-wise calculation
of polychoric correlations}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with following entries:
\item{conttable.skills}{Bivariate contingency table of all skill pairs}
\item{cor.skills}{Tetrachoric correlation matrix for skill
    distribution}
}
\references{
Templin, J., & Henson, R. (2006). Measurement of psychological disorders 
using cognitive diagnosis models. \emph{Psychological Methods}, 
\bold{11}, 287-305.
}
%\author{
%Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# estimate model
d4 <- CDM::din(sim.dino, q.matr = sim.qmatrix)
# compute tetrachoric correlations
skill.cor(d4)
  ## estimated tetrachoric correlations
  ##   $cor.skills
  ##             V1        V2        V3
  ##   V1 1.0000000 0.2567718 0.2552958
  ##   V2 0.2567718 1.0000000 0.9842188
  ##   V3 0.2552958 0.9842188 1.0000000
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Skill correlation}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
