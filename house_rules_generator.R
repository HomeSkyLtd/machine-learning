# Script for generating rules based on house data

source("db.R")
source("train_data.R")

house.generate.rules <- function(house.id, timestamp.start = 0) {
    # Function that generates rules for the specified house
    # taking data starting at the timestamp.start.
    #
    # Args:
    #   house.id: The id of the house to generate the rules
    #   timestamp.start: The timestamp that 
    # Returns:
    #  
    #
    
    
    # First, get all actuators in the input
    metadata <- db.get.metadata(house.id)
    # And all the data
    loadedData <- db.get.training.data(house.id, metadata, timestamp.start)
    # For each actuator, it will generate rules
    for (i in 1:nrow(metadata$command)) {
        action <- metadata$command[1,]
        print(metadata$command)
        # Copy the original data because we will change it
        data <- data.frame(loadedData)
        dataNodes <- metadata$data
        # Select only data of the same room
        if (!is.na(action$room)) {
            dataNodes <- nodes$data[
                !is.na(nodes$data["room"]) & 
                nodes$data["room"] == nodes$command[1,"room"],]
        }
        data <- data[1, paste("data", dataNodes$uniqueId, sep = "_")]
        # Depending on the actuator type, it will apply some cleaning 
        # to the input
        if (action$category == "lightswitch") {
            
        }
        else {
            # Other categories: TODO
        }
        
        # Then, it will clusterize to get a reasonable number of points
        # TODO: clusterize
        
        # Finally, it will run a tree-base algorithm in the input
        train.tree(data, paste("data", action$uniqueId, sep = "_"))
    }
    # Save the list of rules
}
