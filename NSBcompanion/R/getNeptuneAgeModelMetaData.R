getNeptuneAgeModelMetaData <-
function (username="", password="", leg, site, hole){
	if(missing(leg) & missing(site)){stop("Please provide a leg and/or a site.")}
	require(RPostgreSQL)
	con <- dbConnect(dbDriver("PostgreSQL"), user=username, password=password,host="192.168.101.133", dbname="nsb", port="5432")	
	agemodel <- dbReadTable(con, "neptune_age_model")
	agemodel_history <- dbReadTable(con, "neptune_age_model_history")
	hole_summary <- dbReadTable(con, "neptune_hole_summary")
	dbDisconnect(con)
	hole_summary <- hole_summary[,c(2:5)]
	am <- merge(agemodel, agemodel_history, by=c("site_hole","revision_no"))
	am <- merge(am, hole_summary, by="site_hole")
	if(!missing(leg)){am <- am[am$leg==leg,]}
	if(!missing(site)){am <- am[am$site==site,]}
	if(!missing(hole)){am <- am[am$hole==hole,]}
	am
	}
