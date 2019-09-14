changeAgeScale <- function(conn, age_ma, from, to){
  gpts <- dbReadTable(conn, "neptune_gpts")
  gpts1 <- gpts[gpts$scale==from,]
  gpts2 <- gpts[gpts$scale==to,]
  g1_2 <- merge(gpts1,gpts2,"event_id")
  approx(g1_2$age_ma.x,g1_2$age_ma.y,age_ma)$y
}
