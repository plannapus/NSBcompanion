.reverseSyn <- function(conn, genus, species){
  taxonomy <- dbReadTable(conn, "neptune_taxonomy")
  taxonomy <- taxonomy[,colnames(taxonomy)%in%c("taxon_id","taxon_synon_to","species","species_qualifier","genus","genus_qualifier","subspecies","taxon_status")]
  w <- rep(TRUE, nrow(taxonomy))
  if(!missing(genus)){w <- w & toupper(taxonomy$genus)==toupper(genus)}
  if(!missing(species)){w <- w & toupper(taxonomy$species)==toupper(species)}
  r <- resolveSynonymy(conn,taxonomy[w,],verbose=FALSE)
  taxon_id <- taxon_id_i <- unique(r$resolved_taxon_id)
  while(length(taxon_id_i)){
    taxon_id_i <- taxonomy$taxon_id[taxonomy$taxon_synon_to%in%taxon_id_i]
    taxon_id <- c(taxon_id, taxon_id_i)
  }
  taxon_id
}
