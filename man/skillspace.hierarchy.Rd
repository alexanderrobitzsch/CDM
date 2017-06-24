\name{skillspace.hierarchy}
\alias{skillspace.hierarchy}
\alias{skillspace.full}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creation of a Hierarchical Skill Space 
}

\description{
The function \code{skillspace.hierarchy} defines a reduced skill space 
for hierarchies in skills (see e.g. Leighton, Gierl, & Hunka, 2004).
The function \code{skillspace.full} defines a full skill space
for dichotomous skills.
}

\usage{
skillspace.hierarchy(B, skill.names)

skillspace.full(skill.names)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{
A matrix or a string containing restrictions of the hierarchy.
If \code{B} is a \eqn{K \times K} matrix containing where \eqn{K}
denotes the number of skills, then \code{B[ii,jj]=1} means that if an examinee
mastered skill \code{jj}, then he or she should also master skill \code{ii}.

Alternatively, a string can be also conveniently used for defining a
hierarchy (see Examples).
}
  \item{skill.names}{
Vector of names in skills
}
}

\details{
The reduced skill space output can be used as an argument in \code{\link{din}}
or \code{\link{gdina}} to directly test for a hierarchy in attributes.
}

\value{
A list with following entries

\item{R}{Reachability matrix}
\item{skillspace.reduced}{Reduced skill space fulfilling the specified
hierarchy}
\item{skillspace.complete}{Complete skill space}
\item{zeroprob.skillclasses}{Indices of skill patterns in
	\code{skillspace.complete} which were removed for defining
	\code{skillspace.reduced}
}
}

\references{
Leighton, J. P., Gierl, M. J., & Hunka, S. M. (2004). 
The attribute hierarchy method for cognitive assessment: 
A variation on Tatsuoka's rule space approach. 
\emph{Journal of Educational Measurement}, \bold{41}, 205-237.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{din}} (Example 6) for an application of 
\code{skillspace.hierarchy} for model comparisons.

See the \code{\link[GDINA:att.structure]{GDINA::att.structure}} function in the
\pkg{GDINA} package for similar functionality.
}

\examples{
#############################################################################
# EXAMPLE 1: Toy example with 3 skills
#############################################################################

K <- 3 # number of skills
skill.names <- paste0("A" , 1:K )  # names of skills

# create a zero matrix for hierarchy definition
B0 <- 0*diag(K)
rownames(B0) <- colnames(B0) <- skill.names

#*** Model 1: A1 > A2 > A3
B <- B0
B[1,2] <- 1     # A1 > A2
B[2,3] <- 1     # A2 > A3

sp1 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp1$skillspace.reduced
  ##     A1 A2 A3
  ##   1  0  0  0
  ##   2  1  0  0
  ##   4  1  1  0
  ##   8  1  1  1

#*** Model 2:  A1 > A2 and A1 > A3
B <- B0
B[1,2] <- 1     # A1 > A2
B[1,3] <- 1     # A1 > A3

sp2 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp2$skillspace.reduced
  ##     A1 A2 A3
  ##   1  0  0  0
  ##   2  1  0  0
  ##   4  1  1  0
  ##   6  1  0  1
  ##   8  1  1  1

#*** Model 3: A1 > A3 , A2 is not included in a hierarchical way
B <- B0
B[1,3] <- 1     # A1 > A3

sp3 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp3$skillspace.reduced
  ##     A1 A2 A3
  ##   1  0  0  0
  ##   2  1  0  0
  ##   3  0  1  0
  ##   4  1  1  0
  ##   6  1  0  1
  ##   8  1  1  1

#~~~ Hierarchy specification using strings

#*** Model 1: A1 > A2 > A3
B <- "A1 > A2
      A2 > A3"
sp1 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp1$skillspace.reduced

# Model 1 can be also written in one line for B
B <- "A1 > A2 > A3"
sp1b <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp1b$skillspace.reduced

#*** Model 2:  A1 > A2 and A1 > A3
B <- "A1 > A2
      A1 > A3"      
sp2 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp2$skillspace.reduced

#*** Model 3: A1 > A3
B <- "A1 > A3"
sp3 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp3$skillspace.reduced

\dontrun{
#############################################################################
# EXAMPLE 2: Examples from Leighton et al. (2004): Fig. 1 (p. 210)
#############################################################################

skill.names <- paste0("A",1:6) # 6 skills

#*** Model 1: Linear hierarchy (A)
B <- "A1 > A2 > A3 > A4 > A5 > A6"
sp1 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp1$skillspace.reduced

#*** Model 2: Convergent hierarchy (B)
B <- "A1 > A2 > A3
      A2 > A4
      A3 > A5 > A6
      A4 > A5 > A6"
sp2 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp2$skillspace.reduced

#*** Model 3: Divergent hierarchy (C)
B <- "A1 > A2 > A3
      A1 > A4 > A5
      A1 > A4 > A6"            
sp3 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp3$skillspace.reduced

#*** Model 4: Unstructured hierarchy (D)
B <- "A1 > A2 \n A1 > A3 \n A1 > A4 \n A1 > A5 \n A1 > A6"
# This specification of B is equivalent to writing separate lines:
# B <- "A1 > A2
#       A1 > A3
#       A1 > A4
#       A1 > A5
#       A1 > A6"
sp4 <- CDM::skillspace.hierarchy( B=B , skill.names=skill.names )
sp4$skillspace.reduced
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Skill hierarchy}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
