####################################
#Functions


#Requirements: Only one attribute column
# attributes need to have the row beginning attribute:Name

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

#Collapses a string, with the final element being preceeded by the given term
penultPaste <- function(vec, sep = ", ", penultimate = " or "){
	len <- length(vec)
	if(len > 2){
		temp <- paste(vec[- len], collapse = sep)
		out <- paste(temp, vec[len], sep = penultimate)
	}
	if(len == 2) out <- paste(vec, collapse = penultimate)
	out
}

#Collapse a univariate table into a vector organised by name1, value1, name2, val2 etc
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

meanCharacter <- function(dataFrame, column){
	mode <- attr(dataFrame, "Type")[column]
	inVec <- as.vector(dataFrame[ , column], mode = mode)
	
	if(mode == "numeric") outVec <- mean(inVec)
	
	if(mode == "character"){
		if(length(unique(inVec)) == 1) outVec <- unique(inVec) else {
		tab <- table(inVec) / length(inVec)
		outVec <- penultPaste(collapseUniTable(tab))
		}
	}
	outVec
}

#Aggregate characters into species and summarise.
meanCharacter2 <- function(dataFrame, column, sppVector){
	mode <- attr(dataFrame, "Type")[column]
	inVec <- as.vector(dataFrame[ , column], mode = mode)
	
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

meanCharacter2(dat, 31, dat$tagName)
meanCharacter(dat$scutellumShape)

meanCharacter(dat$elytraWidth)
meanCharacter(dat, 41)


#Other functions to develop:
read.SDD --- read files conforming to the SDD standard
write.SDD --- write files conforming to the SDD standard





####################################
#Testing area

setwd("/home/sam/Documents/R/descRibe")

dat <- read.descRibe("sample_dataset.csv")








####################################
#XML
#Examples

library(XML)
reptiles <- xmlTreeParse("/home/sam/Documents/PhD/Data_standards/SDD1.1rev5/examples/SDD_reptiles.xml", useInternalNodes = TRUE)
atom <- xmlTreeParse("/home/sam/Documents/PhD/Data_standards/SDD1.1rev5/examples/SDD-Test-AtomizedSciName.sdd11.xml", useInternalNodes = TRUE)

#Local schema
SDDschema <- xmlSchemaParse("/home/sam/Documents/PhD/Data_standards/SDD1.1rev5/SDD.xsd")
SDDschemaMod <- xmlTreeParse("/home/sam/Documents/PhD/Data_standards/SDD1.1rev5/SDD.xsd")

SDDschemaDoc <- Doctype("/home/sam/Documents/PhD/Data_standards/SDD1.1rev5/SDD.xsd")
#Online
SDDschema <- xmlSchemaParse("http://rs.tdwg.org/UBIF/2006/Schema/1.1/SDD.xsd", xinclude=FALSE)

#Validating XML
xmlSchemaValidate(SDDschema, reptiles)
xmlSchemaValidate(SDDschema, atom)
