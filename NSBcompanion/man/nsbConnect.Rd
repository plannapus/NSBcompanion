\name{nsbConnect}
\alias{nsbConnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Connect to NSB.
}
\description{
Wrapper to connect to NSB database.
}
\usage{
nsbConnect(username, password, intern = FALSE, local = FALSE)
}
\arguments{
  \item{username}{
Username, as character string.
}
  \item{password}{
Username, as character string.
}
  \item{intern}{
Are you currently running this code from inside the Museum für Naturkunde network? Defaults to FALSE.
}
  \item{local}{
Are you trying to connect to a local, offline version of NSB? Defaults to FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A connection object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johan Renaudie.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
nsb <- nsbConnect("guest","arm_aber_sexy")
}
