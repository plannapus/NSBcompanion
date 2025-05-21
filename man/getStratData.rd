\name{getStratData}
\alias{getStratData}

\title{
Get Stratigraphic Data
}
\description{
Retrieve a sratigraphic dataset from Neptune (NSB) database.
}
\usage{
getStratData(conn, hole, scale="Grad12")
}

\arguments{
  \item{conn}{
Connection object (resulting from \code{\link{nsbConnect}}).
}
  \item{hole}{
Character string. Full name of a deep-sea drilling hole, e. g. "119_744A".
}
  \item{scale}{
  Character string. Which age scale to use. Defaults to 'Grad12' (i. e. Gradstein et al. 2012 GPTS). Could be one of 'Berg85', 'CK95', 'Grad08', 'Grad12' and 'GTS2020'.
}

}

\value{
A dataframe downloaded from NSB, similar to one that can be downloaded from the NSB website.
}

\author{
Johan Renaudie
}

\examples{
nsb <- nsbConnect()
getStratData(nsb,'119_744A')

}
