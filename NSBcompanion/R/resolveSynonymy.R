resolveSynonymy <-
function(dataset, username="", password=""){
	require(RPostgreSQL)
	if(.Platform$OS.type=="unix"){
		a <- grep("naturkundemuseum-berlin",system("hostname",intern=TRUE),v=T)
		}else{a <- grep("naturkundemuseum-berlin",system("ipconfig",intern=TRUE),v=T)}
	if(length(a)!=0){host <- '192.168.101.133'}else{host <- '212.201.100.111'}
	con <- dbConnect(dbDriver("PostgreSQL"), user=username, password=password,host=host, dbname="nsb", port="5432")
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
	step <- 1
	while(sum(dataset$resolved_taxon_id%in%syndiat$taxon_id)!=0){
		S <- syndiat[syndiat[,1]%in%dataset$resolved_taxon_id,]
		n <- nrow(S)
		for(i in 1:n){
			dataset$resolved_taxon_id[dataset$resolved_taxon_id==S[i,1]]<-S[i,2]
			cat("Step",step,":",i,"/",n,"\r")
			}
		step <- step+1
		cat("\n")
		}
	cat("0 row done")
	d <- unique(dataset$resolved_taxon_id)
	N <- 0
	for(i in seq_along(d)){
		r <- taxonomy[taxonomy$taxon_id==d[i],]
		if(nrow(r)==1){
			resolved_species[dataset$resolved_taxon_id==d[i]] <- r$species
			resolved_genus[dataset$resolved_taxon_id==d[i]] <- r$genus
			resolved_subspecies[dataset$resolved_taxon_id==d[i]] <- r$subspecies
			}
		N <- N + sum(dataset$resolved_taxon_id==d[i])
		cat("\r",N,"rows done",sep=" ")
		}
	dataset$resolved_species <- resolved_species
	dataset$resolved_genus <- resolved_genus
	dataset$resolved_subspecies <- resolved_subspecies
	dataset
	}
