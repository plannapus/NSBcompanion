\name{findAge}
\alias{findAge}
\alias{findPaleocoordinates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find a sample age and paleocoordinates based on its depth.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findAge(conn, hole_id, depth_mbsf)
findPaleocoordinates(conn, hole_id, age_ma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
Connection object (resulting from \code{\link{nsbConnect}}).
}
  \item{hole_id}{
Hole name (in Leg_SiteHole format, e. g. 120_747A), as a single character string.
}
  \item{depth_mbsf}{
Depth in mbsf. Can be a single numeric value or a vector of numeric values.
}
  \item{age_ma}{
Age in million years. Can be a single numeric value or a vector of numeric values. Typocally the result of \code{findAge}
}
}
\details{
Ages are computed by linear interpolation between tiepoints. Paleolatitude and Paleolongitude are interpolated linearly between paleocoordinates computed for each 1 Ma using Seton et al. 2012 rotation model with GPlates.
}
\value{
\code{findAge} outputs a dataframe with the following columns:
\item{hole_id}{The hole identifier.}
\item{depth_mbsf}{The depth provided.}
\item{age_ma}{The ages in Ma, as computed from the current NSB age model for the given hole.}

\code{findPaleocoordinates} outputs a dataframe with the following columns:
\item{hole_id}{The hole identifier.}
\item{age_ma}{The ages (in Ma) provided.}
\item{paleo_latitude}{The paleolatitude computed for the given age from NSB table neptune_paleogeography, based on Seton et al. 2012 rotation model.}
\item{paleo_longitude}{The paleolongitude computed for the given age from NSB table neptune_paleogeography, based on Seton et al. 2012 rotation model.}
}
\references{
Seton, M., Mueller, R., Zahirovic, S., Gaina, C., Torsvik, T., Shephard, G., Talsma, A., Gurnis, M., Turner, M., Maus, S., and Chandler, M. (2012). Global continental and ocean basin reconstructions since 200Ma. Earth-Science Reviews, 113(3):212-270.
}
\author{
Johan Renaudie.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(RPostgreSQL)
nsb <- nsbConnect("guest","arm_aber_sexy")
ages <- findAge(nsb, "74_525A",seq(0,700,100))
findPaleocoordinates(nsb,"74_525A",ages$age_ma)
#   hole_id    age_ma paleo_latitude paleo_longitude
# 1 74_525A  0.000000      -29.07000       2.9900000
# 2 74_525A  7.316586      -30.04432       1.9756780
# 3 74_525A 17.250107      -31.44501       0.3449808
# 4 74_525A 46.669501      -35.21729      -5.5937301
# 5 74_525A 57.047529      -36.31190      -7.2385553
# 6 74_525A 70.015133      -36.71030      -8.6612107
# 7 74_525A 74.866876      -36.79734      -9.0293501
# 8 74_525A        NA             NA              NA
dbDisconnect(nsb)
}
\keyword{stratigraphy}
\keyword{paleogeography}
