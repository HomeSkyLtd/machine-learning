# File for training data using a tree-based method, returning a tree
# All functions here must be generic. So they can be used in
# any kind of problem, not just in house automation.


library(rpart)
library(rpart.plot)
library(rpart.utils)

source('./util.R')

train.tree <- function (data, y) {
    # Function that trains a tree from data
    # 
    # Args:
    #   data: inptu data. It is a dataframe
    #   y: The column that we want to predict based on the other
    #      columns
    # Returns:
    #   Trained tree
    print(data)
    trained <- do.call("rpart", list(as.formula(paste(y, "~", ".")), data = data))
    rpart.plot(trained, type = 4)
    trained
    
}


interpret.tree = function (output, nodes, command) {
    if (length(output) <= 6)
        return(list())
    data <- strsplit(trimws(output[7:length(output)]), "[)]")
    rules <- NULL
    newTree <- list()
    for (i in 1:length(data)) {
        val <- list()
        temp <- strsplit(data[[i]][2], "(<=)|(>=)|(< )|(> )")
        lhs <- util.extractUniqueId(trimws(temp[[1]][1]))
        rhs <- as.numeric(trimws(strsplit(temp[[1]][2], " ")[[1]][1]))
        node <- nodes$data[nodes$data$uniqueId == lhs,]
        if (nrow(node) == 0) {
            # It is refering to an actuator, ignore
            val$ignore <- TRUE
        }
        else {
            val$ignore <- FALSE
            val$lhs <- paste(node$nodeId, node$id, sep = ".")
            val$rhs <- rhs
            print(data[[i]][2])
            print(temp[[1]][1])
            val$operator <- trimws(substr(strsplit(data[[i]][2], 
                                                    trimws(temp[[1]][1]))[[1]][2], 1, 2))
            if (length(data[[i]]) == 3 && trimws(data[[i]][3]) == "*") {
                val$leaf = TRUE
                # Visit parents
                clauses <- data.frame(lhs = val$lhs, operator = val$operator, rhs = val$rhs)
                currentNum <- as.numeric(trimws(data[[i]][1])) %/% 2
                temp <- strsplit(trimws(strsplit(data[[i]][2], "[(]")[[1]][1]), " ")[[1]]
                value <- temp[length(temp)]
                
                if (value != "NO_ACTION") {
                    value <- as.numeric(temp)
                    while (currentNum > 1) {
                        currentNode <- newTree[[currentNum]]
                        if (!currentNode$ignore) {
                            clauses <- rbind(clauses,
                                                data.frame(lhs = currentNode$lhs, 
                                                        operator = currentNode$operator, 
                                                        rhs = currentNode$rhs))
                        }
                        currentNum <- currentNum %/% 2   
                    }
                    print(clauses)
                    rules <- rbind(rules, data.frame(
                        command = FALSE,
                        controllerId = command$controllerId,
                        accepted = FALSE,
                        clauses = clauses
                    ))
                }
            }
            else
                val$leaf = FALSE
        }
        newTree[[as.numeric(trimws(data[[i]][1]))]] <- val
    }
    rules
}