\name{getNeptuneData}
\alias{getNeptuneData}

\title{
Get Neptune Data
}
\description{
Retrieve a dataset from Neptune (NSB) database.
}
\usage{
getNeptuneData(conn, fossil_group, genus, species,
               age_range, lon_range, lat_range, leg, site, hole, ocean,
               resolve_syn=FALSE, filter_qi=TRUE, filter_on=TRUE, filter_rw=TRUE,
               filter_aq=FALSE, pacman, agescale="Grad12")
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
  \item{leg}{
Character string.
}
  \item{site}{
Integer.
}
  \item{hole}{
Character string.
}
  \item{ocean}{
Accepted values are "ANT" (Antarctic), "ATL" (Atlantic), "PAC" (Pacific), "MED" (Mediterrannean) and "IND" (Indian), or any combinations of them.
}
  \item{resolve_syn}{
  Logical. Resolve taxonomy using TNL. Defaults to FALSE.
}
  \item{filter_qi}{
  Logical. Filter out questionable identifications and taxa invalidly included in the fossil group. Defaults to TRUE.
}
  \item{filter_on}{
  Logical. Filter out open-nomenclature taxa.Defaults to TRUE.
}
  \item{filter_rw}{
  Logical. Filter out problematic samples/occurrences (reworking, ...). Defaults to TRUE.
}
  \item{filter_aq}{
  Filter out sites with age quality. Defaults to FALSE. If not FALSE, set minimum quality for an age model: can be one of 'VP','P','M','G','VG','E'.
}
  \item{pacman}{
  Perform pacman trimming. Numeric value (used for both top and bottom) or vector of 2 numeric values (top and bottom). Leave blank for no trimming.
}
  \item{agescale}{
  Character string. Which age scale to use. Defaults to 'Grad12' (i. e. Gradstein et al. 2012 GPTS). Could be one of 'Berg85', 'CK95', 'Grad08', 'Grad12' and 'GTS2020'.
}

}

\value{
A dataframe downloaded from NSB, similar to one that can be downloaded from the NSB website.
}

\author{
Johan Renaudie
}

\examples{
nsb <- nsbConnect()
# Search for all Neogene siliceous microfossil occurrence in the Indian sector of the Southern Ocean:
n <- getNeptuneData(nsb, fossil_group=c("R","D"),age_range=c(0,23), lon_range=c(20,120), lat_range=c(-90,-40))

# Search for all occurrences of the radiolarian species Cycladophora davisiana:
p <- getNeptuneData(nsb, fossil_group="R",genus="Cycladophora", species="davisiana")
}
