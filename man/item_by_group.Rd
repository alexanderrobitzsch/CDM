%% File Name: item_by_group.Rd
%% File Version: 0.13

\name{item_by_group}
\alias{item_by_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Dataset with Group-Specific Items
}

\description{
Creates a dataset with group-specific items which can be used for multiple
group comparisons.
}

\usage{
item_by_group(dat, group, invariant = NULL, rm.empty = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Dataset with item responses
}
  \item{group}{
Vector of group identifiers
}
\item{invariant}{
	Optional vector of variables which should
not be made group-specific, i.e. which should be treated
as invariant across groups.
}
  \item{rm.empty}{
Logical indicating whether empty columns should be removed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Extended dataset with item responses
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
# EXAMPLE 1: Create dataset with group-specific item responses
#############################################################################

dat <- CDM::data.mg

#-- create dataset with group-specific item responses
dat0 <- CDM::item_by_group( dat = dat[,paste0("I",1:5)] , group=dat$group )

#-- summary statistics
summary(dat0)
colnames(dat0)

#-- set some items to invariant
invariant_items <- c("I1","I4")
dat1 <- CDM::item_by_group( dat = dat[,paste0("I",1:5)] , group=dat$group ,
            invariant=invariant_items)
colnames(dat1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
