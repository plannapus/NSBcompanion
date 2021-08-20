lsr <- function(con, hole_id, mbsf){
  NAM <- dbGetQuery(con,"SELECT a.hole_id, b.revision_no, c.age_quality, b.depth_mbsf, b.age_ma 
                         FROM neptune_hole_summary as a, neptune_age_model as b, neptune_age_model_history as c
                         WHERE a.site_hole=b.site_hole 
                           AND b.site_hole=c.site_hole AND b.revision_no=c.revision_no 
                           AND c.current_flag='Y';")
  NAM <- NAM[NAM$hole_id==hole_id,]
  if(!nrow(NAM)) stop('No age model for this hole.')
  NAM <- NAM[order(NAM$depth_mbsf,NAM$age_ma),]
  LSR <- data.frame(hole=NAM$hole_id[-1],from=embed(NAM$depth_mbsf,2)[,2],to=embed(NAM$depth_mbsf,2)[,1],lsr=diff(NAM$depth_mbsf)/diff(NAM$age_ma))
  LSR$lsr <- LSR$lsr/10   # m/Ma to cm/ka
  LSR <- LSR[LSR$lsr!=0,] # hiatuses
  if(missing(mbsf)){      # if no depth provided, give the full table for the site
    return(LSR)
  }else{
    if(mbsf>max(LSR$to) | mbsf<min(LSR$from)) return(NA) # if out of range, return NA
    return(LSR$lsr[LSR$from <= mbsf & LSR$to >= mbsf])   # else return correct lsr
  }
}
