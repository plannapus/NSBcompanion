sample.depth <- function(conn, samples, intervalsep="/"){
  all_cores <- dbReadTable(conn,"neptune_core")
  nhs <- dbReadTable(conn,"neptune_hole_summary")
  all_cores <- merge(all_cores,nhs)
  
  if(length(grep(",",samples))!=length(samples)){
    s <- samples[!grepl(",",samples)]
    samples[!grepl(",",samples)] <- apply(do.call(rbind,strsplit(s," ")), 1,function(x)paste(x[1],x[2], sep=", "))
  }
  if(length(grep(", ",samples))!=length(samples)){
    s <- samples[!grepl(", ",samples)]
    samples[!grepl(", ",samples)] <- apply(do.call(rbind,strsplit(s,",")), 1,
                                           function(x)paste(x[1],x[2], sep=", "))
  }
  S <- do.call(rbind,strsplit(samples,", "))
  mbsf <- rep(NA,length(samples))
  interval_top <- as.integer(do.call(rbind,strsplit(S[,2],split=intervalsep))[,1])
  sect_temp <- strsplit(S[,1],split="-")
  sect <- sapply(sect_temp,function(x)x[length(x)])
  core <- sapply(sect_temp,function(x)x[length(x)-1])
  site <- sapply(sect_temp,function(x)x[length(x)-2])
  leg <- sapply(sect_temp,function(x)ifelse(length(x)>3,x[length(x)-3],""))
  s <- strsplit(site,"")
  is.hole <- lapply(s,`%in%`,LETTERS)
  hole <- lapply(seq_along(s), function(x)s[[x]][is.hole[[x]]])
  site <- sapply(lapply(seq_along(s), function(x)s[[x]][!is.hole[[x]]]),paste,collapse="")
  for(i in seq_along(samples)){if(length(hole[[i]])==0){hole[[i]] <- ""}}
  hole <- unlist(hole)
  C <- strsplit(core,"")
  is.type <- lapply(C,`%in%`,LETTERS)
  type <- sapply(seq_along(C), function(x)C[[x]][is.type[[x]]])
  core <- sapply(lapply(seq_along(C), function(x)C[[x]][!is.type[[x]]]),paste,collapse="")
  s <- strsplit(sect,"")
  is.type2 <- lapply(s,`%in%`,LETTERS)
  sect <- sapply(lapply(seq_along(s), function(x)s[[x]][!is.type2[[x]]]),paste,collapse="",USE.NAMES=F)
  sect[sapply(is.type2,all)]<-sapply(s[sapply(is.type2,all)],paste,collapse="",USE.NAMES=F)
  sect <- sapply(sect,`[`,1)
  
  S <- data.frame(leg=as.character(leg), site=as.character(site), 
                  hole=as.character(hole), core=as.character(core), 
                  sect=as.character(sect), top=interval_top, stringsAsFactors=FALSE)
  bysite <- split(S, site)
  for(i in seq_along(bysite)){
    b <- bysite[[i]]
    mbsf <- vector(length=nrow(b))
    cs <- all_cores[all_cores$site==b$site[1],]
    for(j in 1:nrow(b)){
      if(b$hole[j]!=""){csh <- cs[cs$hole==b$hole[j],]}else{csh <- cs}
      cshc <- csh[csh$core==b$core[j],]
      if(b$sect[j]=="CC"){
        n <- cshc$core_top_mbsf+cshc$core_length
      }else{
        n <- cshc$core_top_mbsf+1.5*b$sect[j]
      }
      if(is.na(b$top[j]) & b$sect[j]=="CC"){top <- 0}else{top <- b$top[j]}
      if(length(n)) mbsf[j] <- n + top/100
    }
    bysite[[i]]$mbsf <- mbsf
    if(i==1){cat(i,"site done\r")}else{cat(i,"sites done\r")}
  }
  cat("\n")
  unsplit(bysite,site)
}
