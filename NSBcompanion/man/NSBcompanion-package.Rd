\name{NSBcompanion-package}
\alias{NSBcompanion-package}
\alias{NSBcompanion}
\docType{package}
\title{
Some functions to retrieve data from the Neptune Database (Lazarus, 1994).
}
\description{
Some basic functions to retrieve data from Neptune, and a couple of other functions to deal with the preliminary quality control (outliers, synonyms).
}
\details{
\tabular{ll}{
Package: \tab NSBcompanion\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2019-07-09\cr
License: \tab MIT\cr
}
Start with \code{\link{getNeptuneData}} to retrieve a dataset.
Function \code{\link{pacmanNeptune}} will help you perform a trimming of each species range to delete potential outliers and \code{\link{resolveSynonymy}} will resolve the synonymies using Neptune table 'neptune_taxonomy'.
Functions \code{\link{findAge}} and \code{\link{findPaleocoordinates}} will find out the ages and coordinates of samples based on their depth.
}
\author{
Johan Renaudie.
}
\references{
Lazarus, D. B. 1994. Neptune: A marine Micropaleontology Database. Mathematical Geology, 26(7): 817-832.
}

\keyword{ package }
