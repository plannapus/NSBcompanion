findAge <-
function(conn, hole_id, depth_mbsf){
  site_hole <- dbGetQuery(conn,sprintf("SELECT site_hole FROM neptune_hole_summary WHERE hole_id='%s';",hole_id))
  if(!nrow(site_hole)) stop("hole_id not found.")
  revision_no <- dbGetQuery(conn,sprintf("SELECT revision_no FROM neptune_age_model_history WHERE site_hole='%s' AND current_flag='Y';", site_hole))
  if(nrow(revision_no)>0){
    nam <- dbGetQuery(conn,sprintf("SELECT depth_mbsf, age_ma FROM neptune_age_model WHERE site_hole='%s' AND revision_no=%i;", site_hole, revision_no[1,1]))
    nam <- nam[order(nam$depth_mbsf, nam$age_ma),]
    if(nrow(nam)>0){
      age <- approx(nam$depth_mbsf, nam$age_ma, depth_mbsf, ties="ordered")$y
    }else{age <- rep(NA, length(depth_mbsf))}
  }else{age <- rep(NA, length(depth_mbsf))}
  data.frame('hole_id'=hole_id,'depth_mbsf'=depth_mbsf,'age_ma'=age)
}
