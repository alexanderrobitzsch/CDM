%% File Name: gdina.dif.Rd
%% File Version: 0.13

\name{gdina.dif}
\alias{gdina.dif}
\alias{summary.gdina.dif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Differential Item Functioning in the GDINA Model
}
\description{
This function assesses item-wise differential item functioning
in the GDINA model by using the Wald test (de la Torre, 2011; 
Hou, de la Torre & Nandakumar, 2014).
It is necessary that a multiple group GDINA model is previously
fitted.
}
\usage{
gdina.dif(object)

\method{summary}{gdina.dif}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{\link{gdina}}
}
\item{\dots}{Further arguments to be passed}
}

\details{
The p values are also calculated by a Holm adjustment
for multiple comparisons (see \code{p.holm} in
output \code{difstats}).

In the case of two groups, an effect size of differential item functioning
(labeled as \code{UA} (unsigned area) in \code{difstats} value) is defined as
the weighted absolute difference of item response functions. The DIF measure
for item \eqn{j} is defined as
\deqn{UA_j = \sum_l w( \alpha_l ) | P( X_j=1 | \alpha_l , G=1 ) - 
	 P( X_j=1 | \alpha_l , G=2 ) | }
where \eqn{w( \alpha_l ) = [ P( \alpha_l | G=1 ) + P( \alpha_l | G=2 ) ]/2}. 
}
\value{
A list with following entries

 \item{difstats}{Data frame containing results of item-wise Wald tests}
 \item{coef}{Data frame containing all (group-wise) item parameters}
 \item{delta_all}{List of \eqn{\delta} vectors containing all item parameters}
 \item{varmat_all}{List of covariance matrices of all
 \eqn{\delta} item parameters}
 \item{prob.exp.group}{List with groups and items containing expected latent class
 	 sizes and expected probabilities for each group and each item.
 	 Based on this information, effect sizes of differential item
 functioning can be calculated.}

}

\references{
  de la Torre, J. (2011). The generalized {DINA} model framework. 
  \emph{Psychometrika}, \bold{76}, 179-199.
  
  Hou, L., de la Torre, J., & Nandakumar, R. (2014).
  Differential item functioning assessment in cognitive
  diagnostic modeling: Application of the Wald test to
  investigate DIF in the DINA model.
  \emph{Journal of Educational Measurement}, \bold{51}, 98-125.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \code{\link[GDINA:dif]{GDINA::dif}} function in the
\pkg{GDINA} package for similar functionality.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: DIF for DINA simulated data
#############################################################################		

# simulate some data
set.seed(976)
N <- 2000    # number of persons in a group
I <- 9       # number of items
q.matrix <- matrix( 0 , 9,2 )
q.matrix[1:3,1] <- 1
q.matrix[4:6,2] <- 1
q.matrix[7:9,c(1,2)] <- 1
# simulate first group
guess <- rep( .2 , I )
slip <- rep(.1, I)
dat1 <- sim.din( N=N , q.matrix=q.matrix , guess=guess , slip=slip , mean=c(0,0) )$dat
# simulate second group with some DIF items (items 1, 7 and 8)
guess[ c(1,7)] <- c(.3 , .35 )
slip[8] <- .25
dat2 <- sim.din( N=N , q.matrix=q.matrix , guess=guess , slip=slip , mean=c(0.4,.25) )$dat
group <- rep(1:2 , each=N )
dat <- rbind( dat1 , dat2 )

#*** estimate multiple group GDINA model
mod1 <- CDM::gdina( dat , q.matrix=q.matrix , rule="DINA" , group=group )
summary(mod1)

#*** assess differential item functioning
dmod1 <- CDM::gdina.dif( mod1)
summary(dmod1)
  ##     item      X2 df      p p.holm     UA
  ##   1 I001 10.1711  2 0.0062 0.0495 0.0428
  ##   2 I002  1.9933  2 0.3691 1.0000 0.0276
  ##   3 I003  0.0313  2 0.9845 1.0000 0.0040
  ##   4 I004  0.0290  2 0.9856 1.0000 0.0044
  ##   5 I005  2.3230  2 0.3130 1.0000 0.0142
  ##   6 I006  1.8330  2 0.3999 1.0000 0.0159
  ##   7 I007 40.6851  2 0.0000 0.0000 0.1184
  ##   8 I008  6.7912  2 0.0335 0.2346 0.0710
  ##   9 I009  1.1538  2 0.5616 1.0000 0.0180
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GDINA model}
\keyword{Differential item functioning (DIF)}
\keyword{summary}
