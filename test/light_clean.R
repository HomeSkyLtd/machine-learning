# Script to clean the data in light 
#args <- commandArgs(trailingOnly = TRUE)
#if (length(args) != 1) {
#    stop("Usage: Rscript light_clean.R <INPUT_FILE>")
#}

#inputFile <- args[1]
#tbl <- read.table(inputFile)
    
library(tree)
library(rpart)


table <- read.table('test/data/raw/ricardo_lamp.txt', header = TRUE)

# Remove timestamp

table <- select(table, -timestamp)
# Smooth presence
table <- clean.apply(table, clean.smooth.subsequent, 'presence', n = 20)
# Extract action
table <- clean.apply(table, clean.transform.edge, 'light_on',new.column = 'action')

print(head(table))

train_and_print <- function(tbl) {
    
    #tbl <- mutate(tbl, light_on = as.factor(light_on))
    tbl <- mutate(tbl, dummy = rnorm(nrow(tbl)), 
                  dummy2 = rep(1, nrow(tbl)),
                  dummy3 = sample(c(0,1), nrow(tbl), TRUE),
                  dummy4 = sample(c(0,1), nrow(tbl), TRUE),
                  dummy5 = sample(c(0,1), nrow(tbl), TRUE),
                  dummy6 = sample(c(0,1), nrow(tbl), TRUE)
                  )
    withActions <- filter(tbl, action != 'NO_ACTION')
    withoutActions <- filter(tbl, action == 'NO_ACTION')
    print(paste("With actions: ", nrow(withActions)))
    print(paste("Without actions: ", nrow(withoutActions)))
    
    trainData <- rbind(sample_n(withActions,size = nrow(withoutActions),
                                replace = TRUE),
                                sample_n(withoutActions, 
                                             nrow(withoutActions)))
    trained <- randomForest(dummy6~., trainData)
    par(mfrow = c(1,2), xpd = NA)
    plot(trained)
    text(trained)
    trained
}

train_and_print(table)

