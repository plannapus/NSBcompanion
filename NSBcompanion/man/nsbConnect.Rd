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
Are you currently running this code from inside the Museum fuer Naturkunde network? Defaults to FALSE.
}
  \item{local}{
Are you trying to connect to a local, offline version of NSB? Defaults to FALSE.
}
}
\value{
A connection object.
}
\author{
Johan Renaudie.
}
\examples{
nsb <- nsbConnect("guest","arm_aber_sexy")
}
