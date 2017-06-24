\name{equivalent.dina}
\alias{equivalent.dina}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determination of a Statistically Equivalent DINA Model
}
\description{
This function determines a statistically equivalent DINA model
given a Q-matrix using the method of von Davier (2014).
Thereby, the dimension of the skill space is expanded, but in the
reparametrized version, the Q-matrix has a simple structure
or the IRT model is no longer be conjuctive (like in DINA) due
to a redefinition of the skill space.
}
\usage{
equivalent.dina(q.matrix, reparametrization = "B")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q.matrix}{
The Q-matrix (see \code{\link{din}})
}
  \item{reparametrization}{
The used reparametrization (see von Davier, 2014). \code{A} and \code{B}
are possible reparametrizations.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with following entries

  \item{q.matrix}{Original Q-matrix}
  \item{q.matrix.ast}{Reparametrized Q-matrix}  
  \item{alpha}{Original skill space}
  \item{alpha.ast}{Reparametrized skill space}  
}


\references{
von Davier, M. (2014). The DINA model as a constrained general
diagnostic model: Two variants of a model equivalency.
\emph{British Journal of Mathematical and Statistical Psychology},
\bold{67}, 49-71.
}


%\author{
%Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# define a Q-matrix
Q <- matrix( c( 1,0,0 ,  0,1,0 , 
        0,0,1,   1,0,1 ,  1,1,1 ) , byrow=TRUE , ncol=3 )
Q <- Q[ rep(1:(nrow(Q)),each=2) , ]

# equivalent DINA model (using the default reparametrization B)
res1 <- CDM::equivalent.dina( q.matrix=Q )
res1

# equivalent DINA model (reparametrization A)
res2 <- CDM::equivalent.dina( q.matrix=Q , reparametrization="A")
res2

# simulate data
set.seed(789)
D <- ncol(Q)
mean.alpha <- c( -.5 , .5 , 0  )
r1 <- .5 ; Sigma.alpha <- matrix( r1 , D , D ) + diag(1-r1,D)
dat1 <- sim.din( N=2000 , q.matrix=Q , mean = mean.alpha , Sigma=Sigma.alpha )

# estimate DINA model
mod1 <- CDM::din( dat1$dat , q.matrix=Q )

# estimate equivalent DINA model
mod2 <- CDM::din( dat1$dat , q.matrix=res1$q.matrix.ast ,
            skillclasses = res1$alpha.ast)
# restricted skill space must be defined by using the argument 'skillclasses'

# compare model summaries
summary(mod2)
summary(mod1)

# compare estimated item parameters
cbind( mod2$coef , mod1$coef )

# compare estimated skill class probabilities
round( cbind( mod2$attribute.patt, mod1$attribute.patt ) , 4 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{DINA}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
