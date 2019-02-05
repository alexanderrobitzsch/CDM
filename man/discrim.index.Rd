%% File Name: discrim.index.Rd
%% File Version: 0.41

\name{discrim.index}
\alias{discrim.index}
\alias{discrim.index.din}
\alias{discrim.index.gdina}
\alias{discrim.index.mcdina}
\alias{summary.discrim.index}


%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Discrimination Indices at Item-Attribute, Item and Test Level
}


\description{
Computes discrimination indices at the probability metric
(de la Torre, 2008; Henson, DiBello & Stout, 2018).
}

\usage{
discrim.index(object, ...)

\method{discrim.index}{din}(object, \dots)

\method{discrim.index}{gdina}(object, \dots)

\method{discrim.index}{mcdina}(object, \dots)

\method{summary}{discrim.index}(object, file=NULL, digits=3,  \dots)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{\link{din}} or \code{\link{gdina}}.
}
\item{file}{Optional file name for a file in which the summary
output should be sunk}
\item{digits}{Number of digits for rounding}
\item{\dots}{Further arguments to be passed}
}

\details{
If item \eqn{j} possesses \eqn{H_j} categories, the item-attribute
specific discrimination for attribute \eqn{k}
according to Henson et al. (2018) is defined as
\deqn{ DI_{jk}=\frac{1}{2} \max_{ \boldmath{\alpha} }
\left(  \sum_{h=1}^{H_j} | P(X_j=h| \boldmath{\alpha} ) -
P(X_j=h| \boldmath{\alpha}^{(-k)} )
\right )   }
where \eqn{\boldmath{\alpha}^{(-k)}} and \eqn{\boldmath{\alpha}} differ only
in attribute \eqn{k}. The index \eqn{DI_{jk}} can be found as the
value \code{discrim_item_attribute}. The test-level discrimination index
is defined as
\deqn{\overline{DI}=\frac{1}{J} \sum_{j=1}^J \max_k DI_{jk} } and can be found
in \code{discrim_test}.

According to de la Torre (2008) and de la Torre, Rossi and van der Ark (2018),
the item discrimination index (IDI) is defined as
\deqn{IDI_j=\max_{ \boldmath{\alpha}_1,\boldmath{\alpha}_2, h}
     | P(X_j=h| \boldmath{\alpha}_1 ) - P(X_j=h| \boldmath{\alpha}_2 ) |}
    and can be found as \code{idi} in the values list.

}

\value{
A list with following entries

\item{discrim_item_attribute}{Discrimination indices \eqn{DI_{jk}}
    at item level for each attribute}
\item{idi}{Item discrimination index \eqn{IDI_j}}
\item{discrim_test}{Discrimination index at test level}
}

\references{
de la Torre, J. (2008). An empirically based method of Q-matrix validation
for the DINA model: Development and applications.
\emph{Journal of Educational Measurement, 45}, 343-362.
\cr http://dx.doi.org/10.1111/j.1745-3984.2008.00069.x

de la Torre, J., van der Ark, L. A., & Rossi, G. (2018).
Analysis of clinical data from a cognitive diagnosis modeling framework.
\emph{Measurement and Evaluation in Counseling and Development, 51}(4),
281-296.
https://doi.org/10.1080/07481756.2017.1327286

Henson, R., DiBello, L., & Stout, B. (2018). A generalized approach to defining item
discrimination for DCMs.
\emph{Measurement: Interdisciplinary Research and Perspectives, 16}(1), 18-29.
\cr http://dx.doi.org/10.1080/15366367.2018.1436855
}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{cdi.kli}} for discrimination indices based on the
Kullback-Leibler information.

For a fitted model \code{mod} in the \pkg{GDINA} package, discrimination indices can be
extracted by the method \code{extract(mod,"discrim")}
(\code{\link[GDINA:extract]{GDINA::extract}}).
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: DINA and GDINA model
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

#-- fit GDINA and DINA model
mod1 <- CDM::gdina( sim.dina, q.matrix=sim.qmatrix )
mod2 <- CDM::din( sim.dina, q.matrix=sim.qmatrix )

#-- compute discrimination indices
dimod1 <- CDM::discrim.index(mod1)
dimod2 <- CDM::discrim.index(mod2)
summary(dimod1)
summary(dimod2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{Item discrimination}
%\keyword{Cognitive diagnostic discrimination}
%\keyword{summary}
