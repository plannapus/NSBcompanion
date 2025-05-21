\name{sampleDepth}
\alias{sampleDepth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get depth of sample based on its name.
}
\description{
Get depth of sample based on its name.
}
\usage{
sampleDepth(conn, samples, intervalsep="/")
}
\arguments{
  \item{conn}{
Connection object (resulting from \code{\link{nsbConnect}}).
}
  \item{samples}{
Vector of sample names, formatted in the classic DSDP-ODP-IODP format, i. e. Leg-SiteHole-Core-Section,IntervalTop or SiteHole-Core-Section,IntervalTop.
}
  \item{intervalsep}{
Separator when both the top and bottom of the interval is given. Default to "/" (i. e. Leg-SiteHole-Core-Section,IntervalTop/IntervalBottom).
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
sampleDepth(nsb,c("744A-1-1,10","744A-10-6,10","744A-5-CC"))
}
