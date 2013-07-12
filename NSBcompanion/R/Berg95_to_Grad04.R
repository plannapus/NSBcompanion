Berg95_to_Grad04 <-
function(dataset){
	data(agescales)
	dataset$sample_age_ma_Gradstein04 <- approx(x=agescales[,3],y=agescales[,4],xout=dataset$sample_age_ma)$y
	dataset
	}
