\name{getNeptuneAgeModelMetaData}
\alias{getNeptuneAgeModelMetaData}

\title{
Age Model Metadata
}
\description{
Search for the agemodel used in Neptune database for a given site, leg or hole and the corresponding metadata when present.
}
\usage{
getNeptuneAgeModelMetaData(username = "", password = "", leg, site, hole)
}
\arguments{
  \item{username}{
Enter here your username to NSB as a character string. Compulsory.
}
  \item{password}{
Enter here your password to NSB as a character string. Compulsory.
}
  \item{leg}{
Character string.
}
  \item{site}{
Character string.
}
  \item{hole}{
Character string.
}
}

\value{
Returns a dataframe.
}
\author{
Johan Renaudie.
}

\seealso{
\code{\link{getNeptuneData}}, \code{\link{getNeptuneTaxonMetaData}}, \code{\link{Berg95_to_Grad04}}.
}
\examples{
	#Plotting the age-depth plot of ODP Leg 120 Site 748A.
	am748A <- getNeptuneAgeModelMetaData(username="****", password="****", leg="120", site="748", hole="A")
    agedepth <- am748A[,c(3,4)]
    plot(agedepth)
}
