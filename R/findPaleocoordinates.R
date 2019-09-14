findPaleocoordinates <-
function(conn, hole_id, age_ma){
  pg <- dbGetQuery(conn,sprintf("SELECT reconstructed_age_ma, paleo_latitude, paleo_longitude FROM neptune_paleogeography WHERE hole_id='%s';", hole_id))
  pg <- pg[order(pg$reconstructed_age_ma),]
  if(nrow(pg)>0){
    plat <- approx(pg$reconstructed_age_ma, pg$paleo_latitude, age_ma)$y
    plon <- approx(pg$reconstructed_age_ma, pg$paleo_longitude, age_ma)$y
  }else{plat <- plon <- rep(NA, length(age_ma))}
  data.frame('hole_id'=hole_id,'age_ma'=age_ma, 'paleo_latitude'=plat, 'paleo_longitude'=plon)
}
