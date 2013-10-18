getNeptuneTaxonMetaData <-
function(username="", password="", fossil_group, taxon_id){
	require(RPostgreSQL)
	con <- dbConnect(dbDriver("PostgreSQL"), user=username, password=password,host='212.201.100.111', dbname="nsb", port="5432")
	taxonomy <- dbReadTable(con, "neptune_taxonomy")
	dbDisconnect(con)
	if(!missing(fossil_group)){tax <- taxonomy[taxonomy$fossil_group==fossil_group,]}
	if(!missing(taxon_id)){
		tax <- taxonomy[taxonomy$taxon_id%in%taxon_id | taxonomy$taxon_synon_to%in%taxon_id,]
		while(all(!is.na(tax$taxon_synon_to))){
			tax <- rbind(tax,taxonomy[taxonomy$taxon_id%in%tax$taxon_synon_to,])
			tax <- tax[!duplicated(tax),]
			}
		while(nrow(taxonomy[taxonomy$taxon_synon_to%in%tax$taxon_id & !taxonomy$taxon_id%in%tax$taxon_id,])!=0){
			tax <- rbind(tax,taxonomy[taxonomy$taxon_synon_to%in%tax$taxon_id & !taxonomy$taxon_id%in%tax$taxon_id,])
			}
		}
	tax
	}
