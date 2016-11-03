#
# DB functions
#

library(mongolite)
library(jsonlite)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# CONSTANTS

constant.type = function (x) {
    factor(x, levels = c(1, 2, 3, 4),
           labels = c("int", "bool", "real", "string")
    )
}

constant.command.category = function (x) {
    factor(x, levels = c(1, 2, 3, 4, 5, 6, 7),
        labels = c("toggle", "temperature", "fan", "lightswitch",
                   "acmode", "lightintensity", "lightcolor")
    )
}

constant.data.category = function (x) {
    factor(x, levels = c(1, 2, 3, 4, 5, 6, 7),
           labels = c("temperature", "luminance", "presence",
                      "humidity", "pressure", "windspeed", "smoke")
    )
}


db._select = function (conn, ..., sort = NULL) {
    if (is.null(sort)) {
        conn$find(toJSON(list(...), auto_unbox = TRUE))
    }
    else {
        print(toJSON(list(...), auto_unbox = TRUE))
        conn$find(toJSON(list(...), auto_unbox = TRUE),
                  sort = toJSON(sort, auto_unbox = TRUE))
    }
}

db.get.metadata = function (house.id) {
    # Function that gets the house metadata, returning a list with
    # two elements: data and command. Each one is a data.frame
    # containing all data and command available in the house
    #
    # Args:
    #   
    # Returns:
    #
    
    conn <- mongo(db = "server-db", 
                  collection = paste('node', house.id, sep = '_'))
    #TODO: accepted = 1 and alive = 1
    nodes <- db._select(conn, accepted = 1, alive = 1)
    
    sensors <- NULL
    actuators <- NULL
    uniqueId <- 1
    for (i in 1:nrow(nodes)) {
        node <- nodes[i,]
        baseDataFrame = node[c("controllerId", "nodeId")]
        if (!is.null(node$dataType) && !is.null(node$dataType[[1]])) {
            types <- node$dataType[[1]]
            for (j in 1:nrow(types)) {
                type <- types[j,]
                sensors <- rbind(sensors, cbind(baseDataFrame, 
                    data.frame(
                        uniqueId = uniqueId,
                        id = type$id,
                        type = constant.type(type$type),
                        category = constant.data.category(type$dataCategory),
                        min = type$range[[1]][1],
                        max = type$range[[1]][2]
                )))
                uniqueId <- uniqueId + 1
            }
        }
        if (!is.null(node$commandType) && !is.null(node$commandType[[1]])) {
            types <- node$commandType[[1]]
            for (j in 1:nrow(types)) {
                type <- types[j,]
                actuators <- rbind(actuators, cbind(baseDataFrame, 
                    data.frame(
                        uniqueId = uniqueId,
                        id = type$id,
                        type = constant.type(type$type),
                        category = constant.command.category(type$commandCategory),
                        min = type$range[[1]][1],
                        max = type$range[[1]][2]
                )))
                uniqueId <- uniqueId + 1
            }
        }
    }
    list(data = sensors, command = actuators)
}

db._getUniqueId = function(nodes, node) {
    if (!is.na(node$dataId)) {
        select(filter(nodes, controllerId == node$controllerId,
                      nodeId == node$nodeId,
                      id == node$dataId), uniqueId)[[1]]
    }
    else {
        select(filter(nodes, controllerId == node$controllerId,
                      nodeId == node$nodeId,
                      id == node$commandId), uniqueId)[[1]]
    }
}

db.get.training.data = function (house.id, nodes,
                                 timestamp.start = 0,
                                 timestamp.step = 60) {
    # Function that gets all the house data
    #
    # Args:
    #   house.id: The house id
    #   node.ids: Nodes in the format returned by db.get.metadata
    #   start: the starting timestamp that we want the data (default = 0)
    # Returns:
    #   A data.frame with all the data ordered by timestamp
    
    conn <- mongo(db = "server-db", 
                  collection = paste('all_states', house.id, sep = '_'))
    node.ids <- rbind(select(nodes$data, controllerId, nodeId),
                      select(nodes$command, controllerId, nodeId))
    allData <- db._select(conn, "$or" = node.ids,
                          timestamp = list("$gte" = timestamp.start),
                          sort = list(timestamp = 1))
    # Contains current data of all nodes
    currentData = data.frame(timestamp = timestamp.start)
    output <- NULL
    completeData <- FALSE
    for (i in 1:nrow(allData)) {
        measure <- allData[i,]
        uniqueId <- db._getUniqueId(rbind(nodes$data, nodes$command), 
                            measure)
        # Check if the data is complete (all data plus timestamp)
        if (ncol(currentData) == nrow(node.ids) + 1) {
            if (completeData) {
                repeatTimes <- floor((measure$timestamp - currentData$timestamp) / timestamp.step)
                if (repeatTimes > 0) {
                    repeatTimes <- repeatTimes + 1
                    startRow <- nrow(output) + 1
                    output <- rbind(output, currentData[rep(1, repeatTimes),])
                    # Set timestamps
                    if (repeatTimes > 1 && length(startRow) == 1) {
                        output[startRow:nrow(output), "timestamp"] = 
                            #rep(currentData$timestamp, repeatTimes)
                            seq(currentData$timestamp, measure$timestamp - 1, timestamp.step)
                    }
                }
            }
            else
                completeData <- TRUE
        }
        currentData[1,paste(uniqueId)] <- measure$value
        currentData$timestamp <- measure$timestamp
    }
    output
}
