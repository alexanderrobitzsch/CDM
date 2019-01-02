%% File Name: data.sda6.Rd
%% File Version: 0.17

\name{data.sda6}
\alias{data.sda6}
\docType{data}

\title{
Dataset SDA6 (Jurich & Bradshaw, 2014)
}

\description{
This is a simulated dataset of the SDA6 study according to informations
given in Jurich and Bradshaw (2014).
}

\usage{
data(data.sda6)
}



\format{
The datasets contains 17 items observed at 1710 students.

The format is:

\code{List of 2} \cr
\code{ $ data    : num [1:1710, 1:17] 0 1 0 1 0 0 0 0 1 0 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:17] "MCM01" "MCM03" "MCM13" "MCM17" ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ CM: int [1:17] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ II: int [1:17] 0 0 0 0 1 1 1 1 0 0 ...} \cr
\code{  ..$ PP: int [1:17] 0 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ DG: int [1:17] 0 0 0 0 0 0 0 0 0 0 ...} \cr

The meaning of the skills is

\code{CM} -- Critique Methods

\code{II} -- Identify Improvements

\code{PP} -- Protect Participants

\code{DG} -- Discern Generalizability

}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}


\source{
Simulated data
}

\references{
Jurich, D. P., & Bradshaw, L. P. (2014). An illustration of diagnostic
classification modeling in student learning outcomes assessment.
\emph{International Journal of Testing, 14}, 49-72.
}


\examples{
\dontrun{
data(data.sda6, package="CDM")

data <- data.sda6$data
q.matrix <- data.sda6$q.matrix

#*** Model 1a: LCDM with gdina
mod1a <- CDM::gdina( data, q.matrix, rule="ACDM", linkfct="logit",
                  reduced.skillspace=FALSE )
summary(mod1a)

#*** Model 1b: estimate LCDM with gdm
mod1b <- CDM::gdm( data, q.matrix=q.matrix, theta.k=c(0,1) )
summary(mod1b)

#*** Model 2: LCDM with hierarchy II > CM
B <- "II > CM"
ss2 <- CDM::skillspace.hierarchy(B=B, skill.names=colnames(q.matrix ) )
mod2 <- CDM::gdina( data, q.matrix, rule="ACDM", linkfct="logit",
                skillclasses=ss2$skillspace.reduced,
                reduced.skillspace=FALSE )
summary(mod2)

#*** Model 3: LCDM with hierarchy II > CM and DG > CM
B <- "II > CM
      DG > CM"
ss2 <- CDM::skillspace.hierarchy(B=B, skill.names=colnames(q.matrix ) )
mod3 <- CDM::gdina( data, q.matrix, rule="ACDM", linkfct="logit",
               skillclasses=ss2$skillspace.reduced,
               reduced.skillspace=FALSE )
summary(mod3)

# model comparisons
anova(mod1a,mod2)
anova(mod1a,mod3)
# model fit
summary( CDM::modelfit.cor.din(mod1a))
summary( CDM::modelfit.cor.din(mod2) )
summary( CDM::modelfit.cor.din(mod3) )
}
}


%% \keyword{datasets}
