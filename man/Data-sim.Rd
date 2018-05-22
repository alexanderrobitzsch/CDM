%% File Name: Data-sim.Rd
%% File Version: 1.15

\name{Data-sim}

\alias{Data-sim}
\alias{sim.dina}
\concept{sim dina}
\alias{sim.dino}
\concept{sim dino}
\alias{sim.qmatrix}
\concept{sim qmatrix}

\title{
  Artificial Data: DINA and DINO
}

\docType{data}

\description{
  Artificial data: dichotomously coded fictitious answers of 400 respondents
  to 9 items assuming 3 underlying attributes.
}

\usage{
  data(sim.dina)
  data(sim.dino)
  data(sim.qmatrix)
}

\format{
  The \code{sim.dina} and \code{sim.dino} data sets include dichotomous
  answers of \eqn{N=400} respondents to \eqn{J=9} items, thus they are
  \eqn{400 \times 9} data matrices.  For both data sets \eqn{K=3}
  attributes are assumed to underlie the process of responding, stored
  in \code{sim.qmatrix}.

  The \code{sim.dina} data set is simulated according to the DINA condensation
  rule, whereas the \code{sim.dino} data set is simulated according to the
  DINO condensation rule.  The slipping errors for the items 1 to 9 in both
  data sets are \code{0.20, 0.20, 0.20, 0.20, 0.00, 0.50, 0.50, 0.10, 0.03}
  and the guessing errors are \code{0.10, 0.125, 0.15, 0.175, 0.2, 0.225,
  0.25, 0.275, 0.3}.  The attributes are assumed to be mastered with expected
  probabilities of \code{-0.4, 0.2, 0.6}, respectively.  The correlation of
  the attributes is \code{0.3} for attributes 1 and 2, \code{0.4} for
  attributes 1 and 3 and \code{0.1} for attributes 2 and 3.
}


\references{
  Rupp, A. A., Templin, J. L., & Henson, R. A. (2010) \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}. New York: The Guilford
  Press.
}


\section{Example Index}{

%%%%%%% sim.dina
\emph{Dataset} \code{sim.dina}

\code{\link{anova}} (Examples 1, 2),
\code{\link{cdi.kli}} (Example 1),
\code{\link{din}} (Examples 2, 4, 5),
\code{\link{gdina}} (Example 1),
\code{\link{itemfit.sx2}} (Example 2),
\code{\link{modelfit.cor.din}} (Example 1)
\cr

%%%%%%% sim.dino
\emph{Dataset} \code{sim.dino}

\code{\link{cdm.est.class.accuracy}} (Example 1),
\code{\link{din}} (Example 3), \code{\link{gdina}} (Examples 2, 3, 4),
}

%% \examples{
%% ## (2) examples based on dataset sim.dina
%% }

\keyword{datasets}
