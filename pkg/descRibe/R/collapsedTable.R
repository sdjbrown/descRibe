collapsedTable <- function(vec, proportion = TRUE, rounding = 2){
	tab <- table(vec)
	if(proportion) tab <- tab/length(vec)
	len <- length(tab)
	value <- round(tab, rounding)
	out <- vector("character", len)
	for(i in 1:len){
		out[i] <- paste0(names(value[i]), " (", value[i], ")")
	}
	out
}
