\name{nsbConnect}
\alias{nsbConnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Connect to NSB.
}
\description{
Wrapper to connect to an archive of the NSB database.
}
\usage{
nsbConnect(...)
}
\arguments{
  \item{...}{
Argument to be passed to chronosphere's `fetch`, namely a date corresponding to the timestamp of the desired version of the database. Default to the most recent version of the database.
}
}
\value{
A connection object.
}
\author{
Johan Renaudie.
}
\examples{
nsb <- nsbConnect()
nsb2016 <- nsbConnect(ver="2016-02-29")
}
