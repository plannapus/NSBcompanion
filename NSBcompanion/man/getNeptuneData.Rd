\name{getNeptuneData}
\alias{getNeptuneData}

\title{
Get Neptune Data
}
\description{
Retrieve a dataset from Neptune (NSB) database.
}
\usage{
getNeptuneData(conn, fossil_group, age_range, lon_range, lat_range, 
              genus, species, subspecies, taxon_status, 
              leg, site, hole, ocean)
}

\arguments{
  \item{conn}{
Connection object (resulting from \code{\link{nsbConnect}}).
}
  \item{fossil_group}{
Accepted values are "R" (Radiolarians), "F" (Foraminiferans), "D" (Diatoms), "DN" (Dinoflagellates) and "N" (Nanofossils), or any combinations of them.
}
  \item{age_range}{
Vector of two numeric values (min first then max) corresponding to the minimum and maximum age required (in Ma).
}
  \item{lon_range}{
Vector of two numeric values (min first then max) corresponding to the minimum and maximum longitude required (in degrees).
}
  \item{lat_range}{
Vector of two numeric values (min first then max) corresponding to the minimum and maximum latitude required (in degrees).
}
  \item{genus}{
Character string.
}
  \item{species}{
Character string.
}
  \item{subspecies}{
Character string.
}
  \item{taxon_status}{
Accepted values are "V" (Valid), "S" (Synonym), "G" (open-nomenclature), "Q" (Questionable), "U" (Uncertain), "F" (Freshwater, diatoms only), "I" (Invalid, i. e. doesn't belong to the group), or any combinations of them.
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
  \item{ocean}{
Accepted values are "ANT" (Antarctic), "ATL" (Atlantic), "PAC" (Pacific), "MED" (Mediterrannean) and "IND" (Indian), or any combinations of them.
}
}

\value{
A dataframe corresponding to a subset of tables 'neptune_sample', 'neptune_sample_taxa', 'neptune_taxonomy', 'neptune_hole_summary' from NSB.
}

\author{
Johan Renaudie
}

\examples{
  library(RPostgreSQL)
  nsb <- nsbConnect("guest","arm_aber_sexy")
	# Search for all Neogene radiolarian occurrence in the Indian sector of the Southern Ocean:
	n <- getNeptuneData(nsb, fossil_group="R",age_range=c(0,23), lon_range=c(20,120), lat_range=c(-90,-40))
	# Search for all radiolarian and diatom occurrence concerning open-nomenclature taxa in the Atlantic and the Pacific Oceans:
	m <- getNeptuneData(nsb, fossil_group=c("R","D"),taxon_status="G", ocean=c("ATL", "PAC"))
	# Search for all occurrences of the radiolarian species \italic{Cycladophora davisiana}:
	p <- getNeptuneData(nsb, fossil_group="R",genus="Cycladophora", species="davisiana")
}
