getStratData <- function(conn, hole,scale="Grad12"){
  ev <- dbGetQuery(conn,sprintf("SELECT event_id,top_hole_id, bottom_hole_id, 
                                bottom_depth_mbsf, top_depth_mbsf,sample_top, sample_bottom, 
                                comments, quality_flag, source FROM neptune_event WHERE top_hole_id='%s';",hole))
  eid <- ev$event_id
  res <- list()
  g <- dbGetQuery(conn,"SELECT * FROM neptune_gpts_def")
  for(i in seq_along(eid)){
    a <- dbGetQuery(conn,sprintf("SELECT event_id,event_type,event_name,event_group FROM neptune_event_def WHERE event_id=%i;",eid[i]))
    if(a$event_group=="M"){
      b <- dbGetQuery(conn,sprintf("SELECT * FROM neptune_gpts WHERE event_id=%i AND scale='%s';",eid[i],scale))
      a$young_age_ma <- a$old_age_ma <- b$age_ma
      a$calibration_source <- g$long_name[g$scale==scale]
    }else{
      b <- dbGetQuery(conn,sprintf("SELECT * FROM neptune_event_calibration WHERE event_id=%i;",eid[i]))
      if(nrow(b)){
        b <- b[which.max(b$calibration_year),]
        a$young_age_ma <- changeAgeScale(conn, b$young_age_ma, b$calibration_scale, scale)
        a$old_age_ma <- changeAgeScale(conn, b$old_age_ma, b$calibration_scale, scale)
        a$calibration_source <- paste(b$calibration_source,b$calibration_year)
      }else{
        a$young_age_ma <- a$old_age_ma <- a$calibration_source <- NA
      }
    }
    res[[i]] <- a
  }
  a <- do.call(rbind,res)
  m <- merge(ev,a,by="event_id")
  m[order(m$top_depth_mbsf),]
}
