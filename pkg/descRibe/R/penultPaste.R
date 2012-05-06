penultPaste <- function(vec, sep = ", ", penultimate = " or "){
	len <- length(vec)
	if(len > 2){
		temp <- paste(vec[- len], collapse = sep)
		out <- paste(temp, vec[len], sep = penultimate)
	}
	if(len == 2) out <- paste(vec, collapse = penultimate)
	out
}
