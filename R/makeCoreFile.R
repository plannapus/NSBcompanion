makeCoreFile <-
function(conn, site_hole){
  res <- dbGetQuery(conn,sprintf("SELECT a.core as core, a.core_top_mbsf as top, a.core_length as length 
                     FROM neptune_core as a, neptune_hole_summary as b 
                     WHERE a.hole_id=b.hole_id AND b.site_hole='%s';", site_hole))
  if(!nrow(res)) stop('Hole not found.')
  res[,1] <- as.integer(res[,1])
  res[,3] <- res[,2]+res[,3]
  res <- res[order(res[,1]),]
  outputfile <- sprintf("%s.txt",site_hole)
  cat(site_hole,"2\n",sep="\t",file=outputfile)
  cat("CORE\tTOP DEPTH\tBOTTOM DEPTH\n",file=outputfile, append=TRUE)
  for(i in seq_len(nrow(res))){cat(sprintf("%i\t%s\t%s\n",res[i,1],res[i,2],res[i,3]),file=outputfile,append=TRUE)}
}
