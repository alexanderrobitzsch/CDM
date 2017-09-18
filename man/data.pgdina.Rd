%% File Name: data.pgdina.Rd
%% File Version: 0.14
%% File Last Change: 2017-01-31 14:08:30

\name{data.pgdina}
\alias{data.pgdina}
\docType{data}
\title{
Dataset for Polytomous GDINA Model
}
\description{
Dataset for the estimation of the polytomous GDINA model.
}

\usage{data(data.pgdina)}

\format{
The dataset is a list with the item response data and the Q-matrix.	
The format is:

\code{List of 2} \cr
\code{ $ dat     : num [1:1000, 1:30] 1 1 1 1 1 0 1 1 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:30] "I1" "I2" "I3" "I4" ...} \cr
\code{ $ q.matrix: num [1:30, 1:5] 1 0 0 0 0 1 0 0 0 2 ...} \cr
}

\details{
The dataset was simulated by the following \R code:

\code{set.seed(89)} \cr
\code{# define Q-matrix} \cr
\code{Qmatrix <- matrix(c(1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,} \cr
\code{    1,1,2,0,0,0,0,1,2,0,0,0,0,1,2,0,0,0,0,1,1,2,0,0,0,1,2,2,0,1,0,2,} \cr
\code{    1,0,0,1,1,0,2,2,0,0,2,1,0,1,0,0,2,2,1,2,0,0,0,0,0,2,0,0,0,0,0,2,} \cr
\code{    0,0,0,0,0,2,0,0,0,0,0,1,2,0,2,0,0,0,2,0,2,0,0,0,2,0,1,2,0,0,2,0,} \cr
\code{    0,2,0,0,1,1,0,0,1,1,0,1,1,1,0,1,1,1,0,0,0,1,0,1,1,1,0,1,0,1) , } \cr
\code{    nrow=30 , ncol=5 , byrow=TRUE )} \cr
\code{# define covariance matrix between attributes} \cr
\code{Sigma <- matrix(c(1,.6,.6,.3,.3,.6,1,.6,.3,.3,.6,.6,1,} \cr
\code{        .3,.3,.3,.3,.3,1,.8,.3,.3,.3,.8,1) , 5 ,5 , byrow=TRUE )} \cr
\code{# define thresholds for attributes} \cr
\code{q1 <- c( -.5 , .9 )  # attributes 1,...,4} \cr
\code{q2 <- c(0)           # attribute 5} \cr
\code{# number of persons} \cr
\code{N <- 1000} \cr
\code{# simulate latent attributes} \cr
\code{alpha1 <- mvrnorm(n = N,  mu=rep(0,5) , Sigma=Sigma)} \cr
\code{alpha <- 0*alpha1} \cr
\code{for (aa in 1:4)\{} \cr
\code{   alpha[ alpha1[,aa] > q1[1] , aa ] <- 1} \cr
\code{   alpha[ alpha1[,aa] > q1[2] , aa ] <- 2} \cr
\code{               \}} \cr
\code{aa <- 5 ; alpha[ alpha1[,aa] > q2[1] , aa ] <- 1} \cr
\code{# define item parameters} \cr
\code{guess <- c(.07,.01,.34,.07,.11,.23,.27,.07,.08,.34,.19,.19,.25,.04,.34,} \cr
\code{        .03,.29,.05,.01,.17,.15,.35,.19,.16,.08,.18,.19,.07,.17,.34)} \cr
\code{slip <- c(0,.11,.14,.09,.03,.09,.03,.1,.14,.07,.06,.19,.09,.19,.07,.08,} \cr
\code{        .16,.18,.16,.02,.11,.12,.16,.14,.18,.01,.18,.14,.05,.18)} \cr
\code{# simulate item responses} \cr
\code{I <- 30     # number of items} \cr
\code{dat <- latresp <- matrix( 0 , N , I , byrow=TRUE)} \cr
\code{for (ii in 1:I)\{} \cr
\code{#    ii <- 2} \cr
\code{    # latent response matrix} \cr
\code{    latresp[,ii] <- 1*( rowMeans( alpha >= matrix( Qmatrix[ ii , ] , nrow=N , } \cr
\code{                ncol=5 , byrow=TRUE ) ) == 1 )} \cr
\code{    # response probability} \cr
\code{    prob <- ifelse( latresp[,ii] == 1 , 1-slip[ii] , guess[ii] )} \cr
\code{    # simulate item responses} \cr
\code{    dat[,ii] <- 1 * ( runif(N ) < prob )} \cr
\code{                \}} \cr
\code{colnames(dat) <- paste0("I",1:I)} \cr
}

%\source{
%}

%\seealso{
%See Example 7 in \code{\link{gdina}}. 
%}


\references{
Chen, J., & de la Torre, J. (2013). 
A general cognitive diagnosis model for expert-defined polytomous attributes.
\emph{Applied Psychological Measurement}, \bold{37}, 419-437.
}

%\examples{
%data(data.pgdina)
%## maybe str(data.pgdina) ; plot(data.pgdina) ...
%}

\keyword{datasets}
