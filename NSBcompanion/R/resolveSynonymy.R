resolveSynonymy <-
function(dataset, username="", password=""){
	require(RPostgreSQL)
	con <- dbConnect(dbDriver("PostgreSQL"), user=username, password=password,host="192.168.101.133", dbname="nsb", port="5432")
	taxonomy <- dbReadTable(con, "neptune_taxonomy")
	dbDisconnect(con)
	taxonomy <- taxonomy[,colnames(taxonomy)%in%c("taxon_id","taxon_synon_to","species","genus","subspecies")]
	syndiat <- data.frame(taxon_id=as.character(taxonomy$taxon_id), 
					 	  taxon_synon=as.character(taxonomy$taxon_synon_to), stringsAsFactors=FALSE)
	syndiat<-syndiat[!is.na(syndiat$taxon_synon),]
	dataset$resolved_taxon_id <- dataset$taxon_id
	resolved_species <- dataset$species
	resolved_genus <- dataset$genus
	resolved_subspecies <- dataset$subspecies
	while(sum(dataset$resolved_taxon_id%in%syndiat$taxon_id)!=0){
		for(i in 1:nrow(syndiat)){dataset$resolved_taxon_id[dataset$resolved_taxon_id==syndiat[i,1]]<-syndiat[i,2]}
		}
	cat("0 row done")
	for(i in seq_along(dataset$resolved_taxon_id)){
		r <- taxonomy[taxonomy$taxon_id==dataset$resolved_taxon_id[i],]
		if(nrow(r)==1){
			resolved_species[i] <- r$species
			resolved_genus[i] <- r$genus
			resolved_subspecies[i] <- r$subspecies
			}
		cat("\r",i,"rows done",sep=" ")
		}
	dataset$resolved_species <- resolved_species
	dataset$resolved_genus <- resolved_genus
	dataset$resolved_subspecies <- resolved_subspecies
	dataset
	}
