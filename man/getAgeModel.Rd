\name{getAgeModel}
\alias{getAgeModel}

\title{
Get Neptune Data
}
\description{
Retrieve age models from Neptune (NSB) database.
}
\usage{
getAgeModel(conn, hole, current=TRUE, scale="Grad12")
}

\arguments{
  \item{conn}{
Connection object (resulting from \code{\link{nsbConnect}}).
}
  \item{hole}{
Character string. Full name of a deep-sea drilling hole, e. g. "119_744A".
}
  \item{current}{
If TRUE (default) the function will returns a dataframe with the current age model used in the database for the specified hole; if FALSE it returns a list of age models, with their metadata, for that hole.
}
  \item{scale}{
  Character string. Which age scale to use. Defaults to 'Grad12' (i. e. Gradstein et al. 2012 GPTS). Could be one of 'Berg85', 'CK95', 'Grad08', 'Grad12' and 'GTS2020'.
}

}

\value{
If current=TRUE (default) the function will returns a dataframe with the current age model used in the database for the specified hole; if FALSE it returns a list of age models, with their metadata, for that hole.
}

\author{
Johan Renaudie
}

\examples{
nsb <- nsbConnect()

getAgeModel(nsb, "119_744A")

getAgeModel(nsb, "119_744A", FALSE)

getAgeModel(nsb, "119_744A", scale="GTS2020")

}
