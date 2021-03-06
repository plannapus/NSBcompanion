resolveSynonymy <-
function(conn, dataset, verbose=TRUE){
  taxonomy <- dbReadTable(conn, "neptune_taxonomy")
  taxonomy <- taxonomy[,colnames(taxonomy)%in%c("taxon_id","taxon_synon_to","species","species_qualifier","genus","genus_qualifier","subspecies","taxon_status")]
  syndiat <- data.frame(taxon_id=as.character(taxonomy$taxon_id), 
                        taxon_synon=as.character(taxonomy$taxon_synon_to), stringsAsFactors=FALSE)
  syndiat<-syndiat[!is.na(syndiat$taxon_synon),]
  dataset$resolved_taxon_id <- dataset$taxon_id
  resolved_species <- dataset$species
  resolved_genus <- dataset$genus
  resolved_genus_qualifier <- dataset$genus_qualifier
  resolved_species_qualifier <- dataset$species_qualifier
  resolved_subspecies <- dataset$subspecies
  resolved_taxon_status <- dataset$taxon_status
  step <- 1
  while(sum(dataset$resolved_taxon_id%in%syndiat$taxon_id)!=0){
    S <- syndiat[syndiat[,1]%in%dataset$resolved_taxon_id,]
    n <- nrow(S)
    for(i in 1:n){
      dataset$resolved_taxon_id[dataset$resolved_taxon_id==S[i,1]]<-S[i,2]
      if(verbose) cat("Step",step,":",i,"/",n,"\r")
    }
    step <- step+1
    if(verbose) cat("\n")
  }
  if(verbose) cat("0 row done")
  d <- unique(dataset$resolved_taxon_id)
  N <- 0
  for(i in seq_along(d)){
    r <- taxonomy[taxonomy$taxon_id==d[i],]
    if(nrow(r)==1){
      resolved_species[dataset$resolved_taxon_id==d[i]] <- r$species
      resolved_genus[dataset$resolved_taxon_id==d[i]] <- r$genus
      resolved_genus_qualifier[dataset$resolved_taxon_id==d[i]] <- r$genus_qualifier
      resolved_species_qualifier[dataset$resolved_taxon_id==d[i]] <- r$species_qualifier
      resolved_subspecies[dataset$resolved_taxon_id==d[i]] <- r$subspecies
      resolved_taxon_status[dataset$resolved_taxon_id==d[i]] <- r$taxon_status
    }
    N <- N + sum(dataset$resolved_taxon_id==d[i])
    if(verbose) cat("\r",N,"rows done",sep=" ")
  }
  dataset$resolved_genus <- resolved_genus
  dataset$resolved_genus_qualifier <- resolved_genus_qualifier
  dataset$resolved_species <- resolved_species
  dataset$resolved_species_qualifier <- resolved_species_qualifier
  dataset$resolved_subspecies <- resolved_subspecies
  dataset$resolved_taxon_status <- resolved_taxon_status
  dataset
}
