\name{changeAgeScale}
\alias{changeAgeScale}

\title{
  Change Age Scale
}
\description{
 Switch from one GPTS to another.
}
\usage{
  changeAgeScale(conn, age_ma, from, to)
}

\arguments{
  \item{conn}{
    Connection object (resulting from \code{\link{nsbConnect}}).
  }
  \item{age_ma}{
    Vector of numerical ages to transform.
  }
  \item{from}{
    Original GPTS. One of "Berg85","CK95","Grad08" or "Grad12".
  }
  \item{to}{
    Destination GPTS. One of "Berg85","CK95","Grad08" or "Grad12".
  }
}

\value{
A vector of numerical ages on the destinaiton GPTS.
}

\author{
Johan Renaudie
}

\examples{
nsb <- nsbConnect("guest","arm_aber_sexy")
changeAgeScale(nsb,10:20,"Grad12","Berg85")
#[1]  9.172756 10.353858 11.492500 12.783655 13.997523 15.096420 16.246531 17.300253 18.489331
#[10] 19.620703 20.815821
dbDisconnect(nsb)
}
