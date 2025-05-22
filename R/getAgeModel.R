getAgeModel <- function(conn, hole, current=TRUE, scale="Grad12"){
  h <- dbGetQuery(conn, sprintf("SELECT site_hole FROM neptune_hole_summary WHERE hole_id='%s';",hole))
  if(!nrow(h)) stop("Hole unknown.")
  if(current){
    am <- dbGetQuery(conn, sprintf("SELECT a.site_hole, a.age_ma, a.depth_mbsf, a.age_comment
                             FROM neptune_age_model as a, neptune_age_model_history as b
                             WHERE a.site_hole=b.site_hole
                             AND a.site_hole='%s'
                             AND a.revision_no=b.revision_no
                             AND b.current_flag='Y';",h[[1]]))
    am <- am[order(am$depth_mbsf,am$age_ma),]
    if(scale!="Grad12") am$age_ma <- changeAgeScale(conn, am$age_ma, "Grad12", scale)
  }else{
    am <- list()
    amh <- dbGetQuery(conn,sprintf("SELECT site_hole, revision_no, age_quality, interpreted_by, date_worked, current_flag, remark FROM neptune_age_model_history WHERE site_hole='%s';",h[[1]]))
    amh <- amh[order(amh$revision_no,decreasing=TRUE),]
    for(i in 1:nrow(amh)){
      meta <- amh[i,]
      model <- dbGetQuery(conn,sprintf("SELECT site_hole, age_ma, depth_mbsf, age_comment FROM neptune_age_model WHERE revision_no=%i AND site_hole='%s';",amh$revision_no[i], h[[1]]))
      model <- model[order(model$depth_mbsf, model$age_ma),]
      if(scale!="Grad12") model$age_ma <- changeAgeScale(conn, model$age_ma, "Grad12", scale)
      am[[i]] <- list(metadata=meta, age_model=model)
    }
    names(am) <- amh$revision_no
  }
  am
}
