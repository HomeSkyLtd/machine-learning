# Script for generating rules based on house data

source("./db.R")
source("./train.R")
source("./clean/lightswitch.R")
source('./clean/clean_util.R')

house.generate.rules <- function(house.id, timestamp.start = 0) {
    # Function that generates rules for the specified house
    # taking data starting at the timestamp.start.
    #
    # Args:
    #   house.id: The id of the house to generate the rules
    #   timestamp.start: The timestamp that 
    # Returns:
    #   Number of new rules
    #
    
    cat("Loading nodes...\n")
    # First, get all actuators in the input
    metadata <- db.get.metadata(house.id)
    cat(paste("Loaded", (nrow(metadata$data) + nrow(metadata$command)), "nodes\n"))
    # And all the data
    cat("Loading data... \n")
    loadedData <- db.get.training.data(house.id, metadata, timestamp.start)
    cat(paste("Loaded", nrow(loadedData), "data points\n"))
    rulesId <- c()
    # For each actuator, it will generate rules
    cat("Starting processing...\n")
    for (i in 1:nrow(metadata$command)) {
        action <- metadata$command[1,]
        cat(paste("Creating rules for", action$category, 
                    paste("(", action$nodeId, ")\n", sep = "")))
        # Copy the original data because we will change it
        data <- data.frame(loadedData)
        dataNodes <- metadata$data
        # Select only data of the same room
        if (!is.na(action$room)) {
            dataNodes <- metadata$data[
                !is.na(metadata$data["room"]) & 
                    metadata$data["room"] == metadata$command[i,"room"],]
        }
        data <- data[, paste("data", c(dataNodes$uniqueId, action$uniqueId), sep = "_")]
        # Depending on the actuator type, it will apply some cleaning 
        # to the input
        if (action$category == "lightswitch") {
            # Specific cleaning for light
            cat("    Cleaning data...\n")
            data <- light.clean(data, metadata)
            cat("    Cleaned!\n")
        }
        else {
            # Other actuators: Return NULL
            return(NULL);
        }
        
        # Then, it will clusterize to get a reasonable number of points
        cat("    Clusterizing data...\n")
        data <- clean.clusterize_and_balance(data)
        cat("    Clusterized!\n")
        # Finally, it will run a tree-base algorithm in the input
        cat("    Training model...\n")
        trainedModel <- train.tree(data, "action")
        cat("    Trained!\n")
        print(trainedModel)
        cat("    Creating rules...\n")
        # Now, get rules
        rules <- interpret.tree(paste(capture.output(trainedModel)), metadata, action)
        cat("    Created ")
        cat(nrow(rules))
        cat(" rules!\n")
        # Save the list of rules
        if (nrow(rules) > 0) {
            cat("    Saving rules...\n")
            rulesId <- c(rulesId, db.put.rules(house.id, rules))
            cat("    Saved!\n")
        }
    }
    print("Finished!")
    length(rulesId)
}

