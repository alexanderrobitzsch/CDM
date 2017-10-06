%% File Name: CDM-utilities.Rd
%% File Version: 0.08
%% File Last Change: 2017-10-06 10:10:08

\name{CDM-utilities}
\alias{CDM-utilities}
\alias{CDM_require_namespace}
\alias{CDM_rmvnorm}
\alias{cdm_pem_inits}
\alias{cdm_pem_inits_assign_parmlist}
\alias{cdm_pem_acceleration}
\alias{cdm_pem_acceleration_assign_output_parameters}
\alias{cdm_print_summary_data_frame}   


\title{Utility Functions in \pkg{CDM}}


\description{
Utility functions in \pkg{CDM}. 
}

\usage{
## requireNamespace with package message for needed installation
CDM_require_namespace(pkg)    
## print function in summary
cdm_print_summary_data_frame(obji, from=NULL, to=NULL, digits=3)

## mvtnorm::rmvnorm with vector conversion for n=1
CDM_rmvnorm(n, mean=NULL, sigma, ...)

## utility functions for P-EM acceleration
cdm_pem_inits(parmlist)
cdm_pem_inits_assign_parmlist(pem_pars, envir)
cdm_pem_acceleration( iter, pem_parameter_index, pem_parameter_sequence, pem_pars, 
      PEM_itermax , parmlist , ll_fct , ll_args )
cdm_pem_acceleration_assign_output_parameters(res_ll_fct, vars, envir)     
}

\arguments{
\item{pkg}{An \R package}
\item{obji}{Object}
\item{from}{Integer}
\item{to}{Intger}
\item{digits}{Number of digits used for printing}
\item{n}{Integer}
\item{mean}{Mean vector or matrix if separate means for cases are provided. In this case,
    \code{n} can be missing.}
\item{sigma}{Covariance matrix}
\item{\dots}{More arguments to be passed}
\item{parmlist}{List containing parameters}
\item{pem_pars}{Vector containing parameter names}
\item{envir}{Environment}
\item{iter}{Iteration number}
\item{pem_parameter_index}{List with parameter indices}
\item{pem_parameter_sequence}{List with updated parameter sequence}
\item{PEM_itermax}{Maximum number of iterations for PEM}
\item{ll_fct}{Name of log-likelihood function}
\item{ll_args}{Arguments of log-likelihood function}
\item{res_ll_fct}{Result of maximized log-likelihood function}
\item{vars}{Vector containing parameter names}
}

%\details{
%xxx
%%}


% \examples{
% #############################################################################
% }
