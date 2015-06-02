\name{NSBcompanion-package}
\alias{NSBcompanion-package}
\alias{NSBcompanion}
\docType{package}
\title{
Some functions to retrieve data from the Neptune Database (Lazarus, 1994).
}
\description{
Some basic functions to retrieve data from Neptune, and a couple of other functions to deal with the preliminary quality control (outliers, synonyms, age scales).
}
\details{
\tabular{ll}{
Package: \tab NSBcompanion\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2013-10-18\cr
License: \tab GPL\cr
}
Start with \code{\link{getNeptuneData}} to retrieve a dataset. 
\code{\link{getNeptuneAgeModelMetaData}} and \code{\link{getNeptuneTaxonMetaData}} will help you retrieve metadata on a site age model or on a taxon respectively. Function \code{\link{pacmanNeptune}} will help you perform a trimming of each species range to delete potential outliers and \code{\link{resolveSynonymy}} will resolve the synonymies using Neptune table 'neptune_taxonomy'.
}
\author{
Johan Renaudie.
}
\references{
Lazarus, D. B. 1994. Neptune: A marine Micropaleontology Database. Mathematical Geology, 26(7): 817-832.
}

\keyword{ package }

\examples{
	# Retrieve all Neogene radiolarian occurrence in the indian sector of the Southern Ocean.
	# Replace **** with your correct identifiers.
	n <- getNeptuneData(username="****", password="****", fossil_group="R",
						age_range=c(0,23), lon_range=c(20,120), lat_range=c(-90,-40))
						
    sapply(split(n, n$hole_id),nrow)
    #119_736A 119_737A 119_744A 119_745B 119_746A 120_747A 120_748A 120_748B 120_751A   28_265   28_266  28_267B 
    # 524      392      598     1151      719     1161      123      688     1705      637     1011      219 
    #The site with the least number of neogene radiolarians in the Indian sector of the Southern Ocean is odp Leg 120 Site 748A
    #Here is how to plot its age-depth plot:
    am748A <- getNeptuneAgeModelMetaData(username="****", password="****", leg="120", site="748", hole="A")
    agedepth <- am748A[,c(3,4)]
    plot(agedepth)
    
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
