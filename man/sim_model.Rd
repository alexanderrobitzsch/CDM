%% File Name: sim_model.Rd
%% File Version: 0.05

\name{sim_model}
\alias{sim_model}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate an Item Response Model
}

\description{
Simulates an item response model given a fitted object or input of item response
probabilities and skill class probabilities.
}

\usage{
sim_model(object=NULL, irfprob=NULL, theta_index=NULL, prob.theta=NULL,
      data=NULL, N_sim=NULL )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{object}{
Fitted object for which the methods \code{\link{IRT.posterior}},
and \code{\link{IRT.data}} are defined.
}
\item{irfprob}{Array of item response function values (items \eqn{\times} categories
\eqn{\times} skill classes)}
\item{theta_index}{Skill class index for sampling}
\item{prob.theta}{Skill class probabilities}
\item{data}{Original dataset, only relevant for simulating item response pattern
with missing values}
\item{N_sim}{Number of subjects to be simulated}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List containing elements
\item{dat}{Simulated item responses}
\item{theta}{Simulated skill classes}
\item{theta_index}{Corresponding indices to \code{theta}}
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
# EXAMPLE 1: GDINA model simulation
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")
dat <- sim.dina
Q <- sim.qmatrix

# fit DINA model
mod <- CDM::gdina( dat, q.matrix=Q, rule="DINA")
summary(mod)

#** simulate new item responses (N equals observed sample size)
dat1 <- CDM::sim_model(mod)

#*** simulate item responses for N=2000 subjects
dat2 <- CDM::sim_model(mod, N_sim=2000)
str(dat2)

#*** simulate item responses based on input item response probabilities
#*** and theta_index
irfprob <- CDM::IRT.irfprob(mod)
prob.theta <- attr(irfprob, "prob.theta")
TP <- length(prob.theta)
theta_index <- sample(1:TP, size=1000, prob=prob.theta, replace=TRUE )
#-- simulate
dat3 <- CDM::sim_model(irfprob=irfprob, theta_index=theta_index)
str(dat3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Wald test}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
