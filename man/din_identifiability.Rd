%% File Name: din_identifiability.Rd
%% File Version: 0.08

\name{din_identifiability}
\alias{din_identifiability}
\alias{summary.din_identifiability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identifiability Conditions of the DINA Model
}
\description{
Check necessary and sufficient identifiability conditions of the DINA model
according Gu and Xu (xxxx) for a given Q-matrix.
}
\usage{
din_identifiability(q.matrix)

\method{summary}{din_identifiability}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q.matrix}{
Q-matrix
}
\item{object}{Object of class \code{din_identifiability}}
\item{\dots}{Further arguments to be passed}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
List with values
 \item{dina_identified}{Logical indicating whether the DINA model is identified}
 \item{index_single}{Condition 1: vector of logicals indicating whether skills
 are measured by at least one item with a single loading}
 \item{is_three_items}{Condition 2: vector of logicals indicating whether skills
 are measured by at least three items}
\item{submat_distinct}{Condition 3: logical indicating whether all columns
of the submatrix \eqn{Q^\ast} are distinct.}
}


\references{
Gu, Y., & Xu, G. (2018). The sufficient and necessary condition for the identifiability
and estimability of the DINA model. \emph{Psychometrika, xx}(xx), xxx-xxx.
https://doi.org/10.1007/s11336-018-9619-8
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{din.equivalent.class}} for equivalent (i.e., non-distinguishable)
skill classes in the DINA model.
}

\examples{
#############################################################################
# EXAMPLE 1: Some examples of Gu and Xu (2019)
#############################################################################

#* Matrix 1 in Equation (5) of Gu & Xu (2019)
Q1 <- diag(3)
Q2 <- matrix( scan(text="1 1 0 1 0 1 1 1 1 1 1 1"), ncol=3, byrow=TRUE)
Q <- rbind(Q1, Q2)

res <- CDM::din_identifiability(q.matrix=Q)
summary(res)

# remove two items
res <- CDM::din_identifiability(q.matrix=Q[-c(2,5),])
summary(res)

#* Matrix 1 in Equation (6) of Gu & Xu (2019)
Q1 <- diag(3)
Q2 <- matrix( c(1,1,1), nrow=4, ncol=3, byrow=TRUE)
Q <- rbind(Q1, Q2)

res <- CDM::din_identifiability(q.matrix=Q)
summary(res)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
