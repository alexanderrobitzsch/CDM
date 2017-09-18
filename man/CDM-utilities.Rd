%% File Name: CDM-utilities.Rd
%% File Version: 0.03
%% File Last Change: 2017-07-12 10:42:40

\name{CDM-utilities}
\alias{CDM-utilities}
\alias{CDM_require_namespace}
\alias{CDM_rmvnorm}


\title{Utility Functions in \pkg{CDM}}


\description{
Utility functions in \pkg{CDM}. 
}

\usage{
## requireNamespace with package message for needed installation
CDM_require_namespace(pkg)    

## mvtnorm::rmvnorm with vector conversion for n=1
CDM_rmvnorm(n, mean, sigma, ...)
}

\arguments{
\item{pkg}{An \R package}
\item{n}{Integer}
\item{mean}{Mean vector or matrix if separate means for cases are provided. In this case,
    \code{n} can be missing.}
\item{sigma}{Covariance matrix}
\item{\dots}{More arguments to be passed}
}

%\details{
%xxx
%%}


% \examples{
% #############################################################################
% }
