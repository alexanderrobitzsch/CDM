%% File Name: cdm.est.class.accuracy.Rd
%% File Version: 1.353

\name{cdm.est.class.accuracy}
\alias{cdm.est.class.accuracy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classification Reliability in a CDM
}
\description{
This function computes the classification accuracy and
consistency by the method of Cui, Gierl and Chang (2012)
and by simulation. The function computes both statistics
by estimators proposed by Sinharay and Johnson (XXXX; see also
Johnson & Sinharay, 2018) and simulation based estimation.
}
\usage{
cdm.est.class.accuracy(cdmobj, n.sims=0, version=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdmobj}{
Object of class \code{din} or \code{gdina}
}
  \item{n.sims}{
Number of simulated persons. If \code{n.sims=0}, then the number
of persons in the original data is used as the sample size.
In case of missing item responses, for every simulated dataset this sample
size is used.
}
\item{version}{Correct classification reliability statistics can be obtained
    using the default \code{version=2}. For backward compatibility,
    \code{version=1} contains estimators for \pkg{CDM} (<=6.2) which
    have been shown to be biased (Sinharay & Johnson, xxxx).
}
}
\details{
The item parameters and the probability distribution of
latent classes is used as the basis of the simulation.
Accuracy and consistency is estimated for both MLE and MAP
classification estimators. In addition, classification accuracy measures
are available for the separate classification of all skills.
}
\value{
A data frame for MLE, MAP and MAP (Skill 1, ..., Skill \eqn{K})
classification reliability for the whole latent class pattern and
marginal skill classification with following columns:
  \item{Pa_est}{Classification accuracy (Cui et al., 2012) using
  the estimator of Sinharay and Johnson (XXXX)}
  \item{Pa_sim}{Classification accuracy based on simulated data
  (only for \code{din} models)}
  \item{Pc}{Classification consistency (Cui et al., 2012) using
  the estimator of Sinharay and Johnson (XXXX)}
  \item{Pc_sim}{Classification consistency based on simulated data
  (only for \code{din} models)}
}
\references{
Cui, Y., Gierl, M. J., & Chang, H.-H. (2012).
Estimating classification consistency and accuracy for cognitive
diagnostic assessment.
\emph{Journal of Educational Measurement, 49}, 19-38.

Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to assess attribute-level
classification accuracy and consistency for cognitive diagnostic assessments.
\emph{Journal of Educational Measurement, 45}(4), 635-664.

Sinharay, S., & Johnson, M. S. (XXXX). Measures of agreement:
Reliability, classification accuracy, and classification consistency.
In M. von Davier & Y.-S. Lee (Eds.). \emph{Handbook of diagnostic
classification models}. New York: Springer.
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
\dontrun{
#############################################################################
# EXAMPLE 1: DINO data example
#############################################################################

data(sim.dino, package="CDM")
data(sim.qmatrix, package="CDM")

#***
# Model 1: estimate DINO model with din
mod1 <- CDM::din( sim.dino, q.matrix=sim.qmatrix, rule="DINO")
# estimate classification reliability
cdm.est.class.accuracy( mod1, n.sims=5000)

#***
# Model 2: estimate DINO model with gdina
mod2 <- CDM::gdina( sim.dino, q.matrix=sim.qmatrix, rule="DINO")
# estimate classification reliability
cdm.est.class.accuracy( mod2 )

m1 <- mod1$coef[, c("guess", "slip" ) ]
m2 <- mod2$coef
m2 <- cbind( m1, m2[ seq(1,18,2), "est" ],
          1 - m2[ seq(1,18,2), "est" ]  - m2[ seq(2,18,2), "est" ]  )
colnames(m2) <- c("g.M1", "s.M1", "g.M2", "s.M2" )
  ##   > round( m2, 3 )
  ##          g.M1  s.M1  g.M2  s.M2
  ##   Item1 0.109 0.192 0.109 0.191
  ##   Item2 0.073 0.234 0.072 0.234
  ##   Item3 0.139 0.238 0.146 0.238
  ##   Item4 0.124 0.065 0.124 0.009
  ##   Item5 0.125 0.035 0.125 0.037
  ##   Item6 0.214 0.523 0.214 0.529
  ##   Item7 0.193 0.514 0.192 0.514
  ##   Item8 0.246 0.100 0.246 0.100
  ##   Item9 0.201 0.032 0.195 0.032
# Note that s (the slipping parameter) substantially differs for Item4
# for DINO estimation in 'din' and 'gdina'
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Classification reliability}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
