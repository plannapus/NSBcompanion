\name{lsr}
\alias{lsr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear Sedimentation Rate
}
\description{
This function computes the linear sedimentation rate of given samples or along a given hole, according to the age model currently used in NSB for that hole.
}
\usage{
lsr(con, hole_id, depth_mbsf)
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
    Depth in mbsf of the samples (optional). Can only be a single numeric value at this point.
  }
}

\value{
If depth_mbsf is provided, returns a single value of LSR in cm/ka. If not, it returns a table of LSR values from tiepoints to tiepoints (expressed in mbsf).
}
\author{
Johan Renaudie.
}
\note{
NA will be returned for depths out of the age model range.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{findAge}}
}
\examples{
nsb <- nsbConnect("guest","arm_aber_sexy")
lsr(nsb,"71_511")
#     hole    from      to       lsr
# 1 71_511   3.792 114.407 9.6892078
# 2 71_511 114.407 141.108 3.4236309
# 3 71_511 141.108 176.400 1.2990990
# 4 71_511 176.400 194.367 1.0588743
# 6 71_511 194.367 385.500 2.3777188
# 7 71_511 385.500 451.500 0.2477160
# 8 71_511 451.500 512.550 0.4558877
lsr(nsb,"71_511",150)
# [1] 1.299099
lsr(nsb,"71_511",515)
# [1] NA
dbDisconnect(nsb)
