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

#
# INTERNAL FUNCTIONS
#

db._select = function (conn, ..., sort = NULL) {
    # Do select with sort
    if (is.null(sort)) {
        conn$find(toJSON(list(...), auto_unbox = TRUE))
    }
    else {
        conn$find(toJSON(list(...), auto_unbox = TRUE),
                  sort = toJSON(sort, auto_unbox = TRUE))
    }
}

db._getUniqueId = function(nodes, node) {
    # Select unique id of node
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

db.get.metadata = function (house.id) {
    # Function that gets the house metadata, returning a list with house metadata,
    # in other words, information about the house sensors and actuators that are
    # accepted and active.
    #
    # Args:
    #   house.id:   The id of the house
    # Returns:
    #   A list with two elements: data and commands. Each one is a data.frame
    #   containing all data and command available in the house
    
    conn <- mongo(db = "server-db", 
                  collection = paste('node', house.id, sep = '_'))
    
    nodes <- db._select(conn, accepted = 1, alive = 1)
    
    sensors <- NULL
    actuators <- NULL
    uniqueId <- 1
    for (i in 1:nrow(nodes)) {
        node <- nodes[i,]
        # Get node room (all rules will apply only to devices in the same room)
        baseDataFrame = cbind(node[c("controllerId", "nodeId")], node["extra"][[1]]["room"])
        if (!is.null(node$dataType) && !is.null(node$dataType[[1]])) {
            types <- node$dataType[[1]]
            for (j in 1:nrow(types)) {
                type <- types[j,]
                if (constant.type(type$type) == "bool") {
                    sensors <- rbind(sensors, cbind(baseDataFrame, 
                        data.frame(
                            uniqueId = uniqueId,
                            id = type$id,
                            type = constant.type(type$type),
                            category = constant.data.category(type$dataCategory),
                            min = 0,
                            max = 1
                        )))
                }
                else {
                    sensors <- rbind(sensors, cbind(baseDataFrame, 
                        data.frame(
                            uniqueId = uniqueId,
                            id = type$id,
                            type = constant.type(type$type),
                            category = constant.data.category(type$dataCategory),
                            min = type$range[[1]][1],
                            max = type$range[[1]][2]
                    )))
                }
                uniqueId <- uniqueId + 1
            }
        }
        if (!is.null(node$commandType) && !is.null(node$commandType[[1]])) {
            types <- node$commandType[[1]]
            for (j in 1:nrow(types)) {
                type <- types[j,]
                if (constant.type(type$type) == "bool") {
                    actuators <- rbind(actuators, cbind(baseDataFrame, 
                        data.frame(
                            uniqueId = uniqueId,
                            id = type$id,
                            type = constant.type(type$type),
                            category = constant.command.category(type$commandCategory),
                            min = 0,
                            max = 1
                        )))
                }
                else {
                    actuators <- rbind(actuators, cbind(baseDataFrame, 
                        data.frame(
                            uniqueId = uniqueId,
                            id = type$id,
                            type = constant.type(type$type),
                            category = constant.command.category(type$commandCategory),
                            min = type$range[[1]][1],
                            max = type$range[[1]][2]
                    )))
                }
                uniqueId <- uniqueId + 1
            }
        }
    }
    list(data = sensors, command = actuators)
}

db.get.training.data = function (house.id, nodes,
                                 timestamp.start = 0,
                                 timestamp.step = 60) {
    # Function that gets all the house data with columns representing sensor
    # or actuator data and the timestamp. It repeats the data each timestamp.step
    # seconds. All times are represented in seconds.
    #
    # Args:
    #   house.id: The house id
    #   node.ids: Nodes in the format returned by db.get.metadata
    #   timestamp.start: the starting timestamp that we want the data (default = 0)
    #   timestamp.step: this function will repeat rows at this time step,
    #                   for example, if you have a row in 0s and a row in 121s,
    #                   the return will have the first row 3 times, in the case
    #                   you are using the default step (default = 60)
    # Returns:
    #   A data.frame with all the data ordered by timestamp and with n+1
    #   columns, where n columns contain data or command and the remainig
    #   column contains the timestamp
    
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
        print("hi");
        measure <- allData[i,]
        uniqueId <- db._getUniqueId(rbind(nodes$data, nodes$command), 
                            measure)
        # Check if the data is complete (all data plus timestamp)
        if (ncol(currentData) == nrow(node.ids) + 1) {
            print("complete")
            if (completeData) {
                repeatTimes <- floor((measure$timestamp - currentData$timestamp) / 
                                        timestamp.step)
                if (repeatTimes > 0) {
                    repeatTimes <- repeatTimes + 1
                    startRow <- nrow(output) + 1
                    output <- rbind(output, currentData[rep(1, repeatTimes),])
                    # Set timestamps
                    if (repeatTimes > 1 && length(startRow) == 1) {
                        output[startRow:nrow(output), "timestamp"] = 
                            seq(currentData$timestamp, measure$timestamp - 1,
                                timestamp.step)
                    }
                }
            }
            else
                completeData <- TRUE
        }
        #Update current data with value and timestamp
        currentData[1,paste("data", uniqueId, sep = "_")] <- measure$value
        currentData$timestamp <- measure$timestamp
    }
    output
}

db.put.rules = function (house.id, rules) {
    # Function that save the generated rules in the DB
    #
    # Args:
    #   house.id: The id of the house
    #   rules:
    # Returns:
    #
    
    
    #
    #{
    #   command: { 
    #       nodeId: NODE_ID,
    #       commandId: COMMAND_ID
    #       value: VALUE
    #   }
    #   controllerId: CONTROLLER_ID
    #   accepted: 0
    #   clauses: [
    #       {
    #           "lhs": "NODE_ID.DATA_ID"
    #           "operator: "OPERATOR" (can be ==, <=, >=, !=)
    #           "rhs": "NODE_ID.DATA_ID" | VALUE
    #       }
    #  ]
    #}
}
