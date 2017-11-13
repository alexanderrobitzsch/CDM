%% File Name: sequential.items.Rd
%% File Version: 0.13

\name{sequential.items}
\alias{sequential.items}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructing a Dataset with Sequential Pseudo Items for Ordered
Item Responses
}
\description{
This function constructs dichotomous pseudo items from polytomous ordered
items (Tutz, 1997). Using this method, developed test models for dichotomous
data can be applied for polytomous item responses after transforming them
into dichotomous data. See Details for the construction.

Ma and de la Torre (2016) proposed a sequential GDINA model.
Interestingly, the proposed model can be fitted with the
\code{gdina} function in this \pkg{CDM} package while item responses
has to be transformed with the \code{sequential.items} function for
obtaining dichotomous pseudoitems. The Q-matrix for the sequential model of Ma and
de la Torre (2016) can be used in the GDINA model for the
dichotomous pseudoitems. This approach is implemented for automatic
use in \code{\link{gdina}}.
}


\usage{
sequential.items(data)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame with item responses
}
}

\details{
Assume that item \eqn{j} possesses \eqn{K \geq 3} categories. We label these
categories as \eqn{k=0,1,\ldots,K-1}. The original item responses \eqn{X_{nj}}
for person \eqn{n} at item \eqn{j} is then transformed into \eqn{K-1} pseudo
items \eqn{Y_{j1}, \ldots , Y_{j,K-1}}.

The first pseudo item response \eqn{Y_{nj1}} is defined as 1 iff 
\eqn{X_{nj} \geq 1}. The second item responses \eqn{Y_{nj2}} is 1 iff
\eqn{X_{nj} \geq 2}, it is 0 iff \eqn{X_{nj} = 1} and it is missing 
(\code{NA} in the dataset) iff \eqn{X_{nj} = 0}. The construction proceeds
in the same manner for other catgeories (see Tutz, 1997). The pseudo items can be
recognized as 'hurdles' a participant has to master to get a score of \eqn{k}
for the original item.

The pseudo items are treated as conditionally independent which implies that
IRT models or CDMs which assume local independence can be employed for estimation.

For deriving  item response probabilities of the original items from response
probabilities of the pseudo items see Tutz (1997, p. 141ff.).
}


\value{
A list with following entries
	
 \item{dat.expand}{A data frame with dichotomous pseudo items}
 \item{iteminfo}{A data frame containing some item information}
 \item{maxK}{Vector with maximum number of categories per item}
}


\references{
Ma, W., & de la Torre, J. (2016).
A sequential cognitive diagnosis model for polytomous responses.
\emph{British Journal of Mathematical and Statistical Psychology, 69}(3),
253-275.
	
Tutz, G. (1997). Sequential models for ordered responses. 
In W. van der Linden & R. K. Hambleton.
\emph{Handbook of modern item response theory} (pp. 139-152). 
New York: Springer.
}

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
#############################################################################
# EXAMPLE 1: Constructing sequential pseudo items for data.mg
#############################################################################

dat <- CDM::data.mg
items <- colnames(dat)[ which( substring( colnames(dat) ,1,1)=="I" ) ]
##    [1] "I1"  "I2"  "I3"  "I4"  "I5"  "I6"  "I7"  "I8"  "I9"  "I10" "I11"
data <- dat[,items]

# construct sequential dichotomous pseudo items
res <- CDM::sequential.items(data)

# item information table
res$iteminfo
  ##      item itemindex category pseudoitem
  ##   1    I1         1        1         I1
  ##   2    I2         2        1         I2
  ##   3    I3         3        1         I3
  ##   4    I4         4        1    I4_Cat1
  ##   5    I4         4        2    I4_Cat2
  ##   6    I5         5        1    I5_Cat1
  ##   7    I5         5        2    I5_Cat2
  ##     [...]

# extract dataset with pseudo items
dat.expand <- res$dat.expand
colnames(dat.expand)
  ##    [1] "I1"       "I2"       "I3"       "I4_Cat1"  "I4_Cat2"  "I5_Cat1" 
  ##    [7] "I5_Cat2"  "I6_Cat1"  "I6_Cat2"  "I7_Cat1"  "I7_Cat2"  "I7_Cat3" 
  ##   [13] "I8"       "I9"       "I10"      "I11_Cat1" "I11_Cat2" "I11_Cat3"

# compare original items and pseudoitems

#**** Item I1
stats::xtabs( ~ paste(data$I1) + paste(dat.expand$I1) )
  ##                 paste(dat.expand$I1)
  ##   paste(data$I1)     0     1    NA
  ##               0   4339     0     0
  ##               1      0 33326     0
  ##               NA     0     0   578

#**** Item I7

stats::xtabs( ~ paste(data$I7) + paste(dat.expand$I7_Cat1) )
  ##                 paste(dat.expand$I7_Cat1)
  ##   paste(data$I7)     0     1    NA
  ##               0   3825     0     0
  ##               1      0 14241     0
  ##               2      0 14341     0
  ##               3      0  5169     0
  ##               NA     0     0   667

stats::xtabs( ~ paste(data$I7) + paste(dat.expand$I7_Cat2) )
  ##                 paste(dat.expand$I7_Cat2)
  ##   paste(data$I7)     0     1    NA
  ##               0      0     0  3825
  ##               1  14241     0     0
  ##               2      0 14341     0
  ##               3      0  5169     0
  ##               NA     0     0   667

stats::xtabs( ~ paste(data$I7) + paste(dat.expand$I7_Cat3) )
  ##                 paste(dat.expand$I7_Cat3)
  ##   paste(data$I7)     0     1    NA
  ##               0      0     0  3825
  ##               1      0     0 14241
  ##               2  14341     0     0
  ##               3      0  5169     0
  ##               NA     0     0   667

\dontrun{
#*** Model 1: Rasch model for sequentially created pseudo items
mod <- CDM::gdm( dat.expand, irtmodel="1PL" , theta.k=seq(-5,5,len=21) ,
             skillspace="normal" , decrease.increments=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sequential item response model}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
