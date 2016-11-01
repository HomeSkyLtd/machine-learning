# Script for generating rules based on house data

house.genreate.rules.batch <- function(input, nodes.metadata) {
    # Function that generates rules from an input data.frame
    # and its nodes.metadata
    #
    # Args:
    #   input: A data.frame with each line containing a measure 
    #       in a timestamp, containing the columns: nodeId, timestamp,
    #       value, dataId and commandId.
    #   nodes.metadata: A data.frame with each line containing a node
    #       data or command description, contains the columns: nodeId, nodeClass, dataType
    #       and commandType. dataType and commandType are also data.frames,
    #       containing either dataId or commandId
    #                   
    # Returns:
    #   The mongodb id
    #


    # First, get all actuators in the input
    # For each actuator, it will generate rules

        # Depending on the actuator type, it will apply some cleaning 
        # to the input

        # Then, it will clusterize to get a reasonable number of points

        # Finally, it will run a tree-base algorithm in the input

        # Add rules to the return list

    # Return the list
}
