pacmanNeptune <-
function(dataset, top, bottom){
	sp <- split(dataset, dataset$taxon_id)
	for(i in seq_along(sp)){
		sp[[i]] <- sp[[i]][order(sp[[i]]$sample_age_ma),]
		n <- nrow(sp[[i]])
		nb_top <- floor(n*top/100)
		nb_bottom <- floor(n*bottom/100)
		if(nb_bottom>=1){sp[[i]] <- sp[[i]][-((n-nb_bottom+1):n),]}
		if(nb_top>=1){sp[[i]] <- sp[[i]][-(1:nb_top),]}
		}
	res <- do.call(rbind,sp)
	rownames(res)<-NULL
	res
	}
