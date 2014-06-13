getNeptuneData <-
function(username="", password="", fossil_group, 
				age_range, lon_range, lat_range,
				genus, species, subspecies, taxon_status,
				leg, site, hole, ocean){
	require(RPostgreSQL)
	if(.Platform$OS.type=="unix"){
		a <- grep("192\\.168\\.24",system("ifconfig",intern=TRUE),v=T)
		}else{a <- grep("192\\.168\\.24",system("ipconfig",intern=TRUE),v=T)}
	if(length(a)!=0){host <- '192.168.101.133'}else{host <- '212.201.100.111'}
	con <- dbConnect(dbDriver("PostgreSQL"), user=username, password=password,host=host, dbname="nsb", port="5432")	
	sample_taxa <- dbReadTable(con, "neptune_sample_taxa")
	sample <- dbReadTable(con, "neptune_sample")
	taxonomy <- dbReadTable(con, "neptune_taxonomy")
	holesummary <- dbReadTable(con, "neptune_hole_summary")
	dbDisconnect(con)
	nept <- merge(sample_taxa, sample, by="sample_id")
	if(!missing(fossil_group)){
		if(any(fossil_group%in%c("R","F","D","N"))){
			nept <- nept[nept$fossil_group%in%fossil_group,]
			}
		}
	taxonomy <- taxonomy[,colnames(taxonomy)%in%c("taxon_id","genus","genus_qualifier","species","species_qualifier","subspecies","taxon_status")]
	nept <- merge(nept, taxonomy, by="taxon_id")
	if(!missing(genus)){nept <- nept[toupper(nept$genus)%in%toupper(genus),]}
	if(!missing(species)){nept <- nept[toupper(nept$species)%in%toupper(species),]}
	if(!missing(subspecies)){nept <- nept[toupper(nept$subspecies)%in%toupper(subspecies),]}
	if(!missing(taxon_status)){
		if(any(taxon_status%in%c("V","S","Q","U","I","G","F","T","H","B"))){
			nept <- nept[nept$taxon_status%in%taxon_status,]
			}
		}
	if(!missing(age_range)){
		if(length(age_range)==2){
			nept <- nept[!is.na(nept$sample_age_ma),]
			nept <- nept[nept$sample_age_ma >= age_range[1] & nept$sample_age_ma <= age_range[2],]
			}
		}
	nept <- merge(nept, holesummary, by="hole_id")
	if(!missing(lon_range)){
		if(length(lon_range)==2){nept <- nept[nept$longitude >= lon_range[1] & nept$longitude <= lon_range[2],]
			}
		}
	if(!missing(lat_range)){
		if(length(lat_range)==2){
			nept <- nept[nept$latitude >= lat_range[1] & nept$latitude <= lat_range[2],]
			}
		}
	if(!missing(ocean)){
		if(any(ocean%in%c("ANT","ATL","PAC","IND","MED"))){
			nept <- nept[nept$ocean_code%in%ocean,]
			}
		}
	if(!missing(leg)){nept <- nept[nept$leg%in%leg,]}
	if(!missing(site)){nept <- nept[nept$site%in%site,]}
	if(!missing(hole)){nept <- nept[nept$hole%in%hole,]}
	nept <- nept[,!(colnames(nept)%in%c("paleo_longitude", "paleo_latitude", "meters_recovered", "meters_penetrated"))]
	nept
	}
