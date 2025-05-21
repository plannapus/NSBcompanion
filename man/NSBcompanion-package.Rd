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
Version: \tab 3.0\cr
Date: \tab 2025-05-21\cr
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

Renaudie J., Lazarus D.B., Diver P. 2020. NSB (Neptune Sandbox Berlin): An expanded and improved database of marine planktonic microfossil data and deep-sea stratigraphy.. Palaeontologia Electronica, 23(2):a11.
}

\keyword{ package }
