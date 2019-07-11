\name{pacmanNeptune}
\alias{pacmanNeptune}

\title{
Implement PacMan trimming (Lazarus et al. 2012)
}
\description{
Trim the top and the bottom of each species range in order to eliminate potential outliers.
}
\usage{
pacmanNeptune(dataset, top, bottom)
}

\arguments{
  \item{dataset}{
A dataframe, typically the output of \code{\link{getNeptuneData}}.
}
  \item{top}{
Percentage of occurrences to trim at the top of each species range.
}
  \item{bottom}{
Percentage of occurrences to trim at the bottom of each species range.
}
}

\value{
Return a trimmed dataframe.
}
\references{
Lazarus et al. 2012. Pacman profiling: a simple procedure to identify stratigraphic outliers in high-density deep-sea microfossil data. Paleobiology, 38(1): 858-875.
}
\author{
Lazarus et al. 2012 for the algorithm, Johan Renaudie for the code.
}

\examples{
nsb <- nsbConnect("guest","arm_aber_sexy")
n <- getNeptuneData(nsb,fossil_group="R",age_range=c(4,6),ocean="IND")
nrow(n)
#[1] 1725
o <- pacmanNeptune(n, top=10, bottom=5)
nrow(o)
#[1] 1550
dbDisconnect(nsb)
}
