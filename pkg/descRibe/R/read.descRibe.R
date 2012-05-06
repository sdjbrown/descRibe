read.descRibe <- function(file, sep = ";"){
	dataFr <- read.table(file, sep = sep, head = TRUE, na.strings = "", stringsAsFactor = FALSE)
	
	#Determine and format attribute rows
	attributeCol <- grep("attribute", names(dataFr))
	attributeRows	<- grep("attribute", dataFr[ , attributeCol])
	
	attributeVals <- as.character(dataFr[attributeRows, attributeCol])
	attributeVals <- gsub("attribute:", "", attributeVals)
	attrList <- vector("list", length(attributeVals))
	
	for(i in 1:length(attributeVals)){
		attrList[[i]] <- as.character(dataFr[i, - attributeCol])
	}
	names(attrList) <- attributeVals
	
	attrList$Type <- gsub("string", "character", attrList$Type)
	
	#Remove attribute rows
	dataFr <- dataFr[- attributeRows, - attributeCol]
	
	#Assign attributes
	attributes(dataFr) <- c(attributes(dataFr), attrList)
	class(dataFr) <- c("data.frame", "descRibe")
	dataFr
}
