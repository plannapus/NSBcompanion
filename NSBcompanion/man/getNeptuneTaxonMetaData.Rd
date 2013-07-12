\name{getNeptuneTaxonMetaData}
\alias{getNeptuneTaxonMetaData}

\title{
Taxon Metadata
}
\description{
Retrieve informations on a taxon.
}
\usage{
getNeptuneTaxonMetaData(username = "", password = "", fossil_group, taxon_id)
}

\arguments{
 \item{username}{
Enter here your username to NSB as a character string. Compulsory.
}
  \item{password}{
Enter here your password to NSB as a character string. Compulsory.
}
  \item{fossil_group}{
Accepted values are "R" (Radiolarians), "F" (Foraminiferans), "D" (Diatoms) and "N" (Nanofossils).
}
  \item{taxon_id}{
Character string. Any value from column \code{taxon_id} of a dataframe output by function \code{\link{getNeptuneData}}.
}
}

\value{
A dataframe containing informations from table 'neptune_taxonomy'
}

\author{
Johan Renaudie.
}
\seealso{
\code{\link{getNeptuneData}}, \code{\link{getNeptuneTaxonMetaData}}, \code{\lin{resolveSynonymy}}.
}
\examples{
	n <- getNeptuneData(username="****", password="****", fossil_group="R",
						age_range=c(0,23), lon_range=c(20,120), lat_range=c(-90,-40))
	u <- unique(n$taxon_id[n$taxon_status=="U"])
    #[1] "RCERA5023" "RSIPA0016"
    #These two taxa are flagged as Uncertain. Let's see why.
    
    lapply(u, function(x)getNeptuneTaxonMetaData(username="****", password="****", taxon_id=x))
    #[[1]]
	#      taxon_id taxon_status fossil_group        genus    species subspecies taxon_synon_to species_author species_year
	#4682 RCENR5003            S            R Centrobotrys cucullaris       <NA>      RCERA5023           <NA>           NA
	#4690 RCERA0025            Q            R CERATOCYRTIS CUCULLARIS        (Q)      RCERA5023      EHRENBERG         1873
	#4691 RCERA5023            U            R Ceratocyrtis cucullaris       <NA>           <NA>      EHRENBERG         1873
    #	 entered_by date_entered                                                                      taxon_comment
	#4682        dbl         <NA> not in NGDC hole xref or DSDP paleo list. Only other cucullaris is Ceratocyrtis c.
	#4690       <NA>         <NA>                                                                               PALG
	#4691        dbl         <NA>                                              validity of most sp. in genus unknown

	#[[2]]
    #	  taxon_id taxon_status fossil_group      genus  species subspecies taxon_synon_to species_author species_year entered_by
	#6009 RSIPA0016            U            R SIPHOCAMPE ARACHNEA      GROUP           <NA>           <NA>           NA       <NA>
	#     date_entered taxon_comment
	#6009         <NA>          <NA>
}
