\name{makeCoreFile}
\alias{makeCoreFile}
\title{
Make a CORE file for NSB_ADP_wx
}
\description{
Produce a CORE file for use with the NSB_ADP_wx software.
}
\usage{
makeCoreFile(conn, site_hole)
}
\arguments{
  \item{conn}{
Connection object (resulting from \code{\link{nsbConnect}}).
}
  \item{site_hole}{
Hole name (in SiteHole format, e.~g. 747A), as a single character string.
}
}
\value{
Write a tab-delimited file in the current directory.
}
\author{
Johan Renaudie.
}
\examples{
nsb <- nsbConnect()
makeCoreFile(nsb,"747A")
dbDisconnect(nsb)
}
