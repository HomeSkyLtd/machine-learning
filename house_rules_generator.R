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
        print(metadata$command)
        # Copy the original data because we will change it
        data <- data.frame(loadedData)
        # Select only data of the same 
        # Depending on the actuator type, it will apply some cleaning 
        # to the input
        # TODO: cleaning and selecting variables
        
        # Then, it will clusterize to get a reasonable number of points
        # TODO: clusterize
        
        # Finally, it will run a tree-base algorithm in the input
        train.tree(data, paste("data", metadata$command[i,"uniqueId"], sep = "_"))
    }
    # Save the list of rules
}
