%% File Name: CDM-utilities.Rd
%% File Version: 0.35

\name{CDM-utilities}
\alias{CDM-utilities}
\alias{CDM_require_namespace}
\alias{CDM_rmvnorm}
\alias{cdm_pem_inits}
\alias{cdm_pem_inits_assign_parmlist}
\alias{cdm_pem_acceleration}
\alias{cdm_pem_acceleration_assign_output_parameters}
\alias{cdm_print_summary_data_frame}   
\alias{cdm_fa1}
\alias{cdm_print_summary_call}
\alias{cdm_print_summary_computation_time}
\alias{cdm_scad_threshold}
\alias{cdm_lasso_threshold}
\alias{cdm_fit_normal}
\alias{cdm_parameter_regularization}
\alias{cdm_penalty_values}
\alias{cdm_ridge_threshold}
\alias{cdm_elnet_threshold}
\alias{cdm_scadL2_threshold}
\alias{cdm_tlp_threshold}
\alias{cdm_mcp_threshold}


\title{Utility Functions in \pkg{CDM}}


\description{
Utility functions in \pkg{CDM}. 
}

\usage{
## requireNamespace with package message for needed installation
CDM_require_namespace(pkg)    

## print function in summary
cdm_print_summary_data_frame(obji, from=NULL, to=NULL, digits=3, rownames_null=FALSE)
## print summary call
cdm_print_summary_call(object)
## print computation time
cdm_print_summary_computation_time(object)

## mvtnorm::rmvnorm with vector conversion for n=1
CDM_rmvnorm(n, mean=NULL, sigma, ...)
## fit univariate and multivariate normal distribution
cdm_fit_normal(x, w)

## fit unidimensional factor analysis by unweighted least squares 
cdm_fa1(Sigma, method=1, maxit=50, conv=1E-5)

## SCAD thresholding operator
cdm_scad_threshold(beta, lambda, a=3.7)
## lasso thresholding operator
cdm_lasso_threshold(val, eta )
## ridge thresholding operator
cdm_ridge_threshold(beta, lambda)
## elastic net threshold operator
cdm_elnet_threshold( beta, lambda, alpha )
## SCAD-L2 thresholding operator
cdm_scadL2_threshold(beta, lambda, alpha, a=3.7)
## truncated L1 penalty thresholding operator 
cdm_tlp_threshold( beta, tau, lambda )
## MCP thresholding operator
cdm_mcp_threshold(beta, lambda, a=3.7)

## general thresholding operator for regularization
cdm_parameter_regularization(x, regular_type, regular_lam, regular_alpha=NULL, 
         regular_tau=NULL )
## values of penalty function
cdm_penalty_values(x, regular_type, regular_lam, regular_tau=NULL,
       regular_alpha=NULL)

## utility functions for P-EM acceleration
cdm_pem_inits(parmlist)
cdm_pem_inits_assign_parmlist(pem_pars, envir)
cdm_pem_acceleration( iter, pem_parameter_index, pem_parameter_sequence, pem_pars, 
      PEM_itermax, parmlist, ll_fct, ll_args, deviance.history = NULL )
cdm_pem_acceleration_assign_output_parameters(res_ll_fct, vars, envir, update)     
}

\arguments{
\item{pkg}{An \R package}
\item{obji}{Object}
\item{from}{Integer}
\item{to}{Intger}
\item{digits}{Number of digits used for printing}
\item{rownames_null}{Logical}
\item{object}{Object}
\item{n}{Integer}
\item{mean}{Mean vector or matrix if separate means for cases are provided. In this case,
    \code{n} can be missing.}
\item{sigma}{Covariance matrix}
\item{\dots}{More arguments to be passed}
\item{x}{Matrix or vector}
\item{w}{Vector of sampling weights}
\item{Sigma}{Covariance matrix}
\item{method}{Method \code{1} indicates estimation of different
item loadings, method \code{2} estimation of same item loadings.}
\item{maxit}{Maximum number of iterations}
\item{conv}{Convergence criterion}
\item{beta}{Numeric}
\item{lambda}{Regularization parameter}
\item{alpha}{Regularization parameter}
\item{a}{Parameter}
\item{tau}{Regularization parameter}
\item{val}{Numeric}
\item{eta}{Regularization parameter}
\item{regular_type}{Type of regularization}
\item{regular_lam}{Regularization parameter \eqn{\lambda}}
\item{regular_tau}{Regularization parameter \eqn{\tau}}
\item{regular_alpha}{Regularization parameter \eqn{\alpha}}
\item{parmlist}{List containing parameters}
\item{pem_pars}{Vector containing parameter names}
\item{envir}{Environment}
\item{update}{Logical}
\item{iter}{Iteration number}
\item{pem_parameter_index}{List with parameter indices}
\item{pem_parameter_sequence}{List with updated parameter sequence}
\item{PEM_itermax}{Maximum number of iterations for PEM}
\item{ll_fct}{Name of log-likelihood function}
\item{ll_args}{Arguments of log-likelihood function}
\item{deviance.history}{Deviance history, a data frame.}
\item{res_ll_fct}{Result of maximized log-likelihood function}
\item{vars}{Vector containing parameter names}
}

%\details{
%xxx
%%}


% \examples{
% #############################################################################
% }
