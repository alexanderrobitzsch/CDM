%% File Name: itemfit.sx2.Rd
%% File Version: 1.36

\name{itemfit.sx2}
\alias{itemfit.sx2}
\alias{summary.itemfit.sx2}
\alias{plot.itemfit.sx2}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S-X2 Item Fit Statistic for Dichotomous Data
}

\description{
Computes the S-X2 item fit statistic (Orlando & Thissen; 2000, 2003)
for dichotomous data. Note that completely observed data is
necessary for applying this function.
}
\usage{
itemfit.sx2(object, Eik_min=1, progress=TRUE)

\method{summary}{itemfit.sx2}(object, \dots)

\method{plot}{itemfit.sx2}(x, ask=TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
Object of class \code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}},
\code{\link[sirt:rasch.mml]{sirt::rasch.mml}},
\code{\link[sirt:smirt]{sirt::smirt}} or
\code{\link[TAM:tam.mml]{TAM::tam.mml}}
}
\item{x}{
Object of class \code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}},
\code{\link[sirt:rasch.mml]{sirt::rasch.mml}},
\code{\link[sirt:smirt]{sirt::smirt}} or
\code{\link[TAM:tam.mml]{TAM::tam.mml}}
}
  \item{Eik_min}{
The minimum expected cell size for merging score groups.
}
  \item{progress}{
An optional logical indicating whether progress should be displayed.
}
\item{ask}{An optional logical indicating whether every item should be
separately displayed.}
\item{\dots}{Further arguments to be passed}
}

\details{
The S-X2 item fit statistic compares observed and expected proportions
\eqn{O_{jk}} and \eqn{E_{jk}} for item \eqn{j} and
each score group \eqn{k} and forms a chi-square distributed statistic
\deqn{S-X_j^2=\sum_{k=1}^{J-1} N_k \frac{ ( O_{jk} - E_{jk} )^2 }
    { E_{jk} ( 1 - E_{jk} ) } }
The degrees of freedom are \eqn{J-1-P_j} where \eqn{P_j} denotes
the number of estimated item parameters.
}

\value{
A list with following entries

\item{itemfit.stat}{Data frame containing item fit statistics}
\item{itemtable}{Data frame with expected and observed proportions
for each score group and each item. Beside the ordinary p value,
an adjusted p value obtained by correction due to multiple testing
is provided (\code{p.holm}, see
\code{\link[stats:p.adjust]{stats::p.adjust}}.
}
}

\references{
Li, Y., & Rupp, A. A. (2011). Performance of the S-X2 statistic for
full-information bifactor models.
\emph{Educational and Psychological Measurement, 71}, 986-1005.

Orlando, M., & Thissen, D. (2000). Likelihood-based item-fit indices for
dichotomous item response theory models.
\emph{Applied Psychological Measurement, 24}, 50-64.

Orlando, M., & Thissen, D. (2003). Further investigation of the performance of
S-X2: An item fit index for use with dichotomous item response theory models.
\emph{Applied Psychological Measurement, 27}, 289-298.

Zhang, B., & Stone, C. A. (2008). Evaluating item fit for multidimensional
item response models. \emph{Educational and Psychological Measurement,
68}, 181-196.
}

\author{
Alexander Robitzsch
}

\note{
This function does not work properly for multiple groups.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
#############################################################################
# EXAMPLE 1: Items with unequal item slopes
#############################################################################

# simulate data
set.seed(9871)
I <- 11
b <- seq( -1.5, 1.5, length=I)
a <- rep(1,I)
a[4] <- .4
N <- 1000
library(sirt)
dat <- sirt::sim.raschtype( theta=stats::rnorm(N), b=b, fixed.a=a)

#*** 1PL model estimated with gdm
mod1 <- CDM::gdm( dat, theta.k=seq(-6,6,len=21), irtmodel="1PL" )
summary(mod1)
# estimate item fit statistic
fitmod1 <- CDM::itemfit.sx2(mod1)
summary(fitmod1)
  ##       item itemindex   S-X2 df     p S-X2_df RMSEA Nscgr Npars p.holm
  ##   1  I0001         1  4.173  9 0.900   0.464 0.000    10     1  1.000
  ##   2  I0002         2 12.365  9 0.193   1.374 0.019    10     1  1.000
  ##   3  I0003         3  6.158  9 0.724   0.684 0.000    10     1  1.000
  ##   4  I0004         4 37.759  9 0.000   4.195 0.057    10     1  0.000
  ##   5  I0005         5 12.307  9 0.197   1.367 0.019    10     1  1.000
  ##   6  I0006         6 19.358  9 0.022   2.151 0.034    10     1  0.223
  ##   7  I0007         7 14.610  9 0.102   1.623 0.025    10     1  0.818
  ##   8  I0008         8 15.568  9 0.076   1.730 0.027    10     1  0.688
  ##   9  I0009         9  8.471  9 0.487   0.941 0.000    10     1  1.000
  ##   10 I0010        10  8.330  9 0.501   0.926 0.000    10     1  1.000
  ##   11 I0011        11 12.351  9 0.194   1.372 0.019    10     1  1.000
  ##
  ##   -- Average Item Fit Statistics --
  ##   S-X2=13.768 | S-X2_df=1.53
# -> 4th item does not fit to the 1PL model

# plot item fit
plot(fitmod1)

\dontrun{
#*** 2PL model estimated with gdm
mod2 <- CDM::gdm( dat, theta.k=seq(-6,6,len=21), irtmodel="2PL", maxiter=100 )
summary(mod2)
# estimate item fit statistic
fitmod2 <- CDM::itemfit.sx2(mod2)
summary(fitmod2)
  ##       item itemindex   S-X2 df     p S-X2_df RMSEA Nscgr Npars p.holm
  ##   1  I0001         1  4.083  8 0.850   0.510 0.000    10     2  1.000
  ##   2  I0002         2 13.580  8 0.093   1.697 0.026    10     2  0.747
  ##   3  I0003         3  6.236  8 0.621   0.780 0.000    10     2  1.000
  ##   4  I0004         4  6.049  8 0.642   0.756 0.000    10     2  1.000
  ##   5  I0005         5 12.792  8 0.119   1.599 0.024    10     2  0.834
  ##   6  I0006         6 14.397  8 0.072   1.800 0.028    10     2  0.648
  ##   7  I0007         7 15.046  8 0.058   1.881 0.030    10     2  0.639
  ##   [...]
  ##
  ##   -- Average Item Fit Statistics --
  ##   S-X2=10.22 | S-X2_df=1.277

#*** 1PL model estimation in smirt (sirt package)
Qmatrix <- matrix(1, nrow=I, ncol=1 )
mod1a <- sirt::smirt( dat, Qmatrix=Qmatrix )
summary(mod1a)
# item fit statistic
fitmod1a <- CDM::itemfit.sx2(mod1a)
summary(fitmod1a)

#*** 2PL model estimation in smirt (sirt package)
mod2a <- sirt::smirt( dat, Qmatrix=Qmatrix, est.a="2PL")
summary(mod2a)
# item fit statistic
fitmod2a <- CDM::itemfit.sx2(mod2a)
summary(fitmod2a)

#*** 1PL model estimated with rasch.mml2 (in sirt)
mod1b <- sirt::rasch.mml2(dat)
summary(mod1b)
# estimate item fit statistic
fitmod1b <- CDM::itemfit.sx2(mod1b)
summary(fitmod1b)

#*** 1PL estimated in TAM
library(TAM)
mod1c <- TAM::tam.mml( resp=dat )
summary(mod1c)
# item fit
summary( CDM::itemfit.sx2( mod1c) )
# conversion to mirt object
library(sirt)
library(mirt)
cmod1c <- sirt::tam2mirt( mod1c )
# item fit in mirt
mirt::itemfit( cmod1c$mirt )

#*** 2PL estimated in TAM
mod2c <- TAM::tam.mml.2pl( resp=dat )
summary(mod2c)
# item fit
summary( CDM::itemfit.sx2( mod2c) )
# conversion to mirt object and item fit in mirt
cmod2c <- sirt::tam2mirt( mod2c )
mirt::itemfit( cmod2c$mirt )

# estimation in mirt
mod1d <- mirt::mirt( dat, 1, itemtype="Rasch" )
mirt::itemfit( mod1d )    # compute item fit

#############################################################################
# EXAMPLE 2: Item fit statistics sim.dina dataset
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

#*** Model 1: DINA model (correctly specified model)
mod1 <- CDM::din( data=sim.dina, q.matrix=sim.qmatrix )
summary(mod1)
# item fit statistic
summary( CDM::itemfit.sx2( mod1 ) )
  ##   -- Average Item Fit Statistics --
  ##   S-X2=7.397 | S-X2_df=1.233

#*** Model 2: Mixed DINA/DINO model
#***  1th item is misspecified according to DINO rule
I <- ncol(CDM::sim.dina)
rule <- rep("DINA", I )
rule[1] <- "DINO"
mod2 <- CDM::din( data=CDM::sim.dina, q.matrix=CDM::sim.qmatrix, rule=rule)
summary(mod2)
# item fit statistic
summary( CDM::itemfit.sx2( mod2 ) )
  ##   -- Average Item Fit Statistics --
  ##   S-X2=9.925 | S-X2_df=1.654

#*** Model 3: Additive GDINA model
mod3 <- CDM::gdina( data=CDM::sim.dina, q.matrix=CDM::sim.qmatrix, rule="ACDM")
summary(mod3)
# item fit statistic
summary( CDM::itemfit.sx2( mod3 ) )
  ##   -- Average Item Fit Statistics --
  ##   S-X2=8.416 | S-X2_df=1.678
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Item fit}
%% \keyword{summary}
%% \keyword{plot}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
