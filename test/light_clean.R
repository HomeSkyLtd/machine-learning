# Script to clean the data in light 
#args <- commandArgs(trailingOnly = TRUE)
#if (length(args) != 1) {
#    stop("Usage: Rscript light_clean.R <INPUT_FILE>")
#}

#inputFile <- args[1]
#tbl <- read.table(inputFile)
    
library(tree)
library(rpart)
library(rpart.plot)
library(RWeka)
library(partykit)
library(ROSE)
library(C50)

source('./data_clean_sample_util.R')


light._extractUniqueId = function (name) {
    return(as.numeric(strsplit(name, "_")[[1]][2]))
}

light.clean <- function(data, nodes) {
    cols <- colnames(data)
    cleanedData <- data
    for (i in 1:length(cols)) {
        uniqueId <- light._extractUniqueId(cols[i])
        nodeData <- nodes$data[nodes$data$uniqueId == uniqueId,]
        if (nrow(nodeData) == 1) {
            # Smooth all presence data    
            if (nodeData$category == 'presence') {
                cleanedData <- clean.apply(cleanedData, clean.smooth.subsequent, cols[i], n = 20)
            }
        }
        else {
            # It is actuator
        }
        
        
    }
    cleanedData
}

table <- read.table('test/data/raw/henrique.txt', header = TRUE)

# Remove timestamp

table <- select(table, -timestamp)
# Smooth presence
table <- clean.apply(table, clean.smooth.subsequent, 'presence', n = 20)
# Extract action
table <- clean.apply(table, clean.transform.edge, 'light_on',new.column = 'action')

print(head(table))

train_and_print <- function(tbl) {
    
    #tbl <- mutate(tbl, light_on = as.factor(light_on))
    #tbl <- mutate(tbl, dummy = rnorm(nrow(tbl)), 
    #              dummy2 = rep(1, nrow(tbl)),
    #              dummy3 = sample(c(0,1), nrow(tbl), TRUE),
    #              dummy4 = sample(c(0,1), nrow(tbl), TRUE),
    #              dummy5 = sample(c(0,1), nrow(tbl), TRUE),
    #              dummy6 = sample(c(0,1), nrow(tbl), TRUE)
    #              )
    withActions <- filter(tbl, action != 'NO_ACTION')
    withoutActions <- filter(tbl, action == 'NO_ACTION')
    uniqueWithActions <- unique(withActions)
    uniqueWithoutActions <- unique(withoutActions)
    print(paste("With actions: ", nrow(withActions)))
    print(paste("Unique With actions: ", nrow(uniqueWithActions)))
    print(paste("Without actions: ", nrow(withoutActions)))
    print(paste("Unique Without actions: ", nrow(uniqueWithoutActions)))
    
    trainData <- rbind(rep(withActions, nrow(withoutActions) / nrow(withActions)),
                       withoutActions)
    #trainData
    ## make classifier 
    trained <- rpart(action ~ ., trainData, cost = , predictArgs=c(type='class'))
    rpart.plot(trained, type = 4)
    printcp(trained)
    #par(mfrow = c(1,2), xpd = NA)
    #plot(trained)
    #text(trained)
    trained
}

train_and_print(table)

