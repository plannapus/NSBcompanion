getNeptuneTaxonMetaData <-
function(username="", password="", fossil_group, taxon_id){
	require(RPostgreSQL)
	con <- dbConnect(dbDriver("PostgreSQL"), user=username, password=password,host="212.201.100.111", dbname="nsb", port="5432")
	taxonomy <- dbReadTable(con, "neptune_taxonomy")
	dbDisconnect(con)
	if(!missing(fossil_group)){tax <- taxonomy[taxonomy$fossil_group==fossil_group,]}
	if(!missing(taxon_id)){tax <- taxonomy[taxonomy$taxon_id%in%taxon_id | taxonomy$taxon_synon_to%in%taxon_id,]}
	tax
	}
