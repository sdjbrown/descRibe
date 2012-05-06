meanCharacter <- function(descRibe, column, sppVector){
	mode <- attr(descRibe, "Type")[column]
	inVec <- as.vector(descRibe[ , column], mode = mode)
	
	if(mode == "numeric") outList <- lapply(split(inVec, sppVector), mean)
	
	if(mode == "character"){
		inList <- split(inVec, sppVector)
		outList <- lapply(inList, function(xx){
			aa <- unique(xx)
			if(length(aa) == 1) out <- aa else{
				out1 <- collapsedTable(xx)
				out <- penultPaste(out1)
			}
			out
		})
	}
	outList
}
