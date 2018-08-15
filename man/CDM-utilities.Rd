%% File Name: CDM-utilities.Rd
%% File Version: 0.64

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


\alias{cdm_parameter_regularization}
\alias{cdm_penalty_values}

\alias{cdm_penalty_threshold_scad}
\alias{cdm_penalty_threshold_lasso}
\alias{cdm_fit_normal}
\alias{cdm_penalty_threshold_ridge}
\alias{cdm_penalty_threshold_elnet}
\alias{cdm_penalty_threshold_scadL2}
\alias{cdm_penalty_threshold_tlp}
\alias{cdm_penalty_threshold_mcp}
\alias{cdm_parameter_regularization}

\alias{CDM_rbind_fill}
\alias{cdm_matrix2}
\alias{cdm_matrix1}
\alias{abs_approx}
\alias{abs_approx_D1}

\alias{cdm_matrixstring}

\alias{cdm_calc_information_criteria}
\alias{cdm_print_summary_information_criteria}

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
cdm_print_summary_call(object, call_name="call")
## print computation time
cdm_print_summary_computation_time(object, time_name="time", time_start="s1",
         time_end="s2")

## string vector of matrix entries
cdm_matrixstring( matr, string )

## mvtnorm::rmvnorm with vector conversion for n=1
CDM_rmvnorm(n, mean=NULL, sigma, ...)
## fit univariate and multivariate normal distribution
cdm_fit_normal(x, w)

## fit unidimensional factor analysis by unweighted least squares
cdm_fa1(Sigma, method=1, maxit=50, conv=1E-5)

## another rbind.fill implementation
CDM_rbind_fill( x, y )
## fills a vector row-wise into a matrix
cdm_matrix2( x, nrow )
## fills a vector column-wise into a matrix
cdm_matrix1( x, ncol )

## SCAD thresholding operator
cdm_penalty_threshold_scad(beta, lambda, a=3.7)
## lasso thresholding operator
cdm_penalty_threshold_lasso(val, eta )
## ridge thresholding operator
cdm_penalty_threshold_ridge(beta, lambda)
## elastic net threshold operator
cdm_penalty_threshold_elnet( beta, lambda, alpha )
## SCAD-L2 thresholding operator
cdm_penalty_threshold_scadL2(beta, lambda, alpha, a=3.7)
## truncated L1 penalty thresholding operator
cdm_penalty_threshold_tlp( beta, tau, lambda )
## MCP thresholding operator
cdm_penalty_threshold_mcp(beta, lambda, a=3.7)

## general thresholding operator for regularization
cdm_parameter_regularization(x, regular_type, regular_lam, regular_alpha=NULL,
         regular_tau=NULL )
## values of penalty function
cdm_penalty_values(x, regular_type, regular_lam, regular_tau=NULL,
       regular_alpha=NULL)
## thresholding operators regularization
cdm_parameter_regularization(x, regular_type, regular_lam, regular_alpha=NULL, 
       regular_tau=NULL)
       
## utility functions for P-EM acceleration
cdm_pem_inits(parmlist)
cdm_pem_inits_assign_parmlist(pem_pars, envir)
cdm_pem_acceleration( iter, pem_parameter_index, pem_parameter_sequence, pem_pars,
      PEM_itermax, parmlist, ll_fct, ll_args, deviance.history=NULL )
cdm_pem_acceleration_assign_output_parameters(res_ll_fct, vars, envir, update)

## approximation of absolute value function and its derivative
abs_approx(x, eps=1e-05)
abs_approx_D1(x, eps=1e-05)

## information criteria
cdm_calc_information_criteria(ic)
cdm_print_summary_information_criteria(object, digits_crit = 0, digits_penalty = 2) 
}

\arguments{
\item{pkg}{An \R package}
\item{obji}{Object}
\item{from}{Integer}
\item{to}{Integer}
\item{digits}{Number of digits used for printing}
\item{rownames_null}{Logical}
\item{call_name}{Character}
\item{time_name}{Character}
\item{time_start}{Character}
\item{time_end}{Character}
\item{matr}{Matrix}
\item{string}{String}
\item{object}{Object}
\item{n}{Integer}
\item{mean}{Mean vector or matrix if separate means for cases are provided. In this case,
    \code{n} can be missing.}
\item{sigma}{Covariance matrix}
\item{\dots}{More arguments to be passed}
\item{x}{Matrix or vector}
\item{y}{Matrix or vector}
\item{w}{Vector of sampling weights}
\item{nrow}{Integer}
\item{ncol}{Integer}
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
\item{eps}{Numeric}
\item{ic}{List}
\item{digits_crit}{Integer}
\item{digits_penalty}{Integer}
}


%\details{
%xxx
%%}


% \examples{
% #############################################################################
% }
