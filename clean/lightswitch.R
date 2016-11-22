# Script to clean the data in light 
#args <- commandArgs(trailingOnly = TRUE)
#if (length(args) != 1) {
#    stop("Usage: Rscript light_clean.R <INPUT_FILE>")
#}

#inputFile <- args[1]
#tbl <- read.table(inputFile)
    
library(rpart)
library(rpart.plot)

source('./clean/clean_util.R')
source('./util.R')


light.clean <- function(data, nodes) {
    cols <- colnames(data)
    cleanedData <- data
    for (i in 1:length(cols)) {
        uniqueId <- util.extractUniqueId(cols[i])
        nodeData <- nodes$data[nodes$data$uniqueId == uniqueId,]
        if (nrow(nodeData) == 1) {
            # Smooth all presence data    
            if (nodeData$category == 'presence') {
                cleanedData <- clean.apply(cleanedData, clean.smooth.subsequent, cols[i], n = 20)
            }
        }
        else {
            # It is actuator
            cleanedData <- clean.apply(cleanedData, clean.transform.edge, cols[i], new.column = 'action')
        }
        
        
    }
    cleanedData
}

