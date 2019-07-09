\name{resolveSynonymy}
\alias{resolveSynonymy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Resolve synonymy.
}
\description{
Resolve the synonymies using the result of the IODP Paleontology Coordination Group's TNL project (see Lazarus et al. 2015).
}
\usage{
resolveSynonymy(conn, dataset)
}
\arguments{
  \item{conn}{
Connection object (resulting from \code{\link{nsbConnect}}).
}
  \item{dataset}{
A dataset retrieved from NSB database using function \code{\link{getNeptuneData}}.
}
}
\value{
The function outputs a dataframe similar to the input dataset with extra columns: resolved_taxon_id, resolved_genus, resolved_species and resolved_subspecies.
}
\references{
Lazarus, D., Suzuki, N., Caulet, J.-P., Nigrini, C., Goll, I., Goll, R., Dolven, J. K., Diver, P., and Sanfilippo, A. (2015). An evaluated list of cenozic-recent radiolarian species names (poly- cystinea), based on those used in the dsdp, odp and iodp deep-sea drilling programs. Zootaxa, 3999(3):301– 333.

}
\author{
Johan Renaudie.
}
\examples{
library(RPostgreSQL)
nsb <- nsbConnect("guest","arm_aber_sexy")
data <- getNeptuneData(nsb,fossil_group="R",age_range=0:1)
res <- resolveSynonymy(nsb, data)
# Step 1 : 118 / 118
# Step 2 : 12 / 12
#  7517 rows done
length(unique(res$taxon_id))
#[1] 386
length(unique(res$resolved_taxon_id))
#[1] 299
dbDisconnect(nsb)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{taxonomy}
