getNeptuneData <-
function(conn, fossil_group, genus, species,
               age_range, lon_range, lat_range, leg, site, hole, ocean,
               resolve_syn=FALSE, filter_qi=TRUE, filter_on=TRUE, filter_rw=TRUE,
               filter_aq=FALSE, pacman, agescale="Grad12"){
  sample <- dbReadTable(conn, "neptune_sample")
  if(!missing(fossil_group)){
    if(any(fossil_group%in%c("R","F","D","N","DN"))){
      sample <- sample[sample$fossil_group%in%fossil_group,]
    }
  }
  if(filter_rw) sample <- sample[!sample$sample_status%in%c("c","w"),]
  if(agescale!="Grad12") sample$sample_age_ma <- changeAgeScale(conn, sample$sample_age_ma, "Grad12", agescale)
  if(!missing(age_range)){
    if(length(age_range)==2){
      sample <- sample[!is.na(sample$sample_age_ma),]
      sample <- sample[sample$sample_age_ma >= age_range[1] & sample$sample_age_ma <= age_range[2],]
    }
  }
  
  sample_taxa <- dbReadTable(conn, "neptune_sample_taxa")
  nept <- merge(sample,sample_taxa, by="sample_id")
  if(filter_rw) nept <- nept[is.na(nept$taxon_qualifier),]
  
  taxonomy <- dbReadTable(conn, "neptune_taxonomy")
  taxonomy <- taxonomy[,colnames(taxonomy)%in%c("taxon_id","genus","genus_qualifier","species","species_qualifier","subspecies","taxon_status")]
  nept <- merge(nept, taxonomy, by="taxon_id")
  if(!missing(genus)|!missing(species)){
    if(!missing(genus) & missing(species)){ids <- .reverseSyn(conn, genus=genus)}
    else if(missing(genus) & !missing(species)){ids <- .reverseSyn(conn, species=species)}
    else if(!missing(genus) & !missing(species)){ids <- .reverseSyn(conn, genus=genus, species=species)}
    nept <- nept[nept$taxon_id%in%ids,]
  }
  if(filter_on) nept <- nept[nept$taxon_status!="G",]
  if(filter_qi) nept <- nept[!nept$taxon_status%in%c("Q","I","U","O","H"),]
  
  holesummary <- dbReadTable(conn,"neptune_hole_summary")
  nept <- merge(nept, holesummary, by="hole_id")
  if(!missing(lon_range)){
    if(length(lon_range)==2){
      nept <- nept[nept$longitude >= lon_range[1] & nept$longitude <= lon_range[2],]
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
  if(!missing(leg)) nept <- nept[nept$leg%in%leg,]
  if(!missing(site)) nept <- nept[nept$site%in%site,]
  if(!missing(hole)) nept <- nept[nept$hole%in%hole,]
  
  sod <- dbReadTable(conn, "neptune_sod_off")
  sod <- sod[,c("dataset_id","source_citation")]
  nept <- merge(nept, sod, by="dataset_id", all.x=TRUE)
  types <- c("VP","P","M","G","VG","E")
  if(filter_aq%in%types){
    good <- types[which(types%in%filter_aq):6]
    amh <- dbReadTable(conn,"neptune_age_model_history")
    amh <- amh[amh$current_flag=="Y",c("site_hole","age_quality")]
    nept <- merge(nept, amh, by="site_hole",all.x=TRUE)
    nept <- nept[nept$age_quality%in%good,]
  }
  if(resolve_syn) nept <- resolveSynonymy(conn, nept,verbose=FALSE)
  if(!missing(pacman)){
    if(length(pacman)==2){nept <- pacmanNeptune(nept, pacman[1], pacman[2])}
    if(length(pacman)==1){nept <- pacmanNeptune(nept, pacman, pacman)}
  }
  final_columns <- c('taxon_id', 'genus', 'genus_qualifier', 'species', 'species_qualifier', 'subspecies', 'taxon_status', 'fossil_group', 'sample_id', 'leg', 'site', 'hole_id', 'core', 'section_number', 'sect_interval_top', 'sample_depth_mbsf', 'latitude', 'longitude', 'sample_age_ma', 'sample_group_abundance', 'sample_preservation', 'taxon_abundance','paleo_latitude','paleo_longitude','longhurst_code','dataset_id','source_citation')
  if(resolve_syn) final_columns <- c('resolved_taxon_id','resolved_genus', 'resolved_genus_qualifier', 'resolved_species', 'resolved_species_qualifier', 'resolved_subspecies', 'resolved_taxon_status', final_columns[8:27])
  nept[,final_columns]
}
