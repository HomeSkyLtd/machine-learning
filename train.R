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
    #   data: input data. It is a dataframe
    #   y: The column that we want to predict based on the other
    #      columns
    # Returns:
    #   Trained tree
   

    trained <- do.call("rpart", 
                        list(as.formula(paste(y, "~", ".")), 
                             data = data,
                             method = "class",
                             parms=list(split="information"),
                             control=rpart.control(usesurrogate = 0, maxsurrogate = 0)))
    # Use this code to plot the tree
    #rpart.plot(trained, type = 4, extra = 104, tweak=1.1)
    trained
    
}

interpret.tree = function (output, nodes, command) {
    # Function that receives the tree output and
    # convert it to rules
    # 
    # Args:
    #   output: The tree string output (as printed by rpart)
    #   nodes: Nodes list, as returned by db.get.metadata
    #   command: The command of nodes list that is being aimed
    #            by the learning algorithm
    # Returns:
    #   data.frame containing the rules


    if (length(output) <= 6)
        return(list())
    data <- strsplit(trimws(output[7:length(output)]), "[)]")
    rules <- data.frame()
    newTree <- list()
    for (i in 1:length(data)) {
        val <- list()
        temp <- strsplit(data[[i]][2], "(<=)|(>=)|(< )|(> )")
        lhs <- util.extractUniqueId(trimws(temp[[1]][1]))
        rhs <- as.numeric(trimws(strsplit(temp[[1]][2], " ")[[1]][1]))
        node <- nodes$data[nodes$data$uniqueId == lhs,]
        if (is.null(node) || nrow(node) == 0) {
            # It is refering to an actuator, ignore
            val$ignore <- TRUE
        }
        else {
            val$ignore <- FALSE
            val$lhs <- paste(node$nodeId, node$id, sep = ".")
            val$rhs <- rhs
            #print(data[[i]][2])
            #print(temp[[1]][1])
            val$operator <- trimws(substr(strsplit(data[[i]][2], 
                                                    trimws(temp[[1]][1]))[[1]][2], 1, 2))
            if (node$type == "bool") {
                if (val$operator == ">=" || val$operator == ">") {
                    val$rhs <- 1
                }
                else {
                    val$rhs <- 0
                }
                val$operator <- "=="
            }
            if (length(data[[i]]) == 3 && trimws(data[[i]][3]) == "*") {
                val$leaf = TRUE
                # Visit parents
                clauses <- data.frame(lhs = val$lhs, operator = val$operator, rhs = val$rhs)
                currentNum <- as.numeric(trimws(data[[i]][1])) %/% 2
                temp <- strsplit(trimws(strsplit(data[[i]][2], "[(]")[[1]][1]), " ")[[1]]
                value <- temp[length(temp)]
                if (value != "NO_ACTION") {
                    value <- as.numeric(value)
                    while (currentNum > 1) {
                        currentNode <- newTree[[currentNum]]
                        if (!currentNode$ignore) {
                            if (sum(clauses$lhs == currentNode$lhs &
                                        clauses$operator == currentNode$operator) > 0) {
                                #clauses <- data.frame()
                                #break
                            }
                            clauses <- rbind(clauses,
                                                data.frame(lhs = currentNode$lhs, 
                                                        operator = currentNode$operator, 
                                                        rhs = currentNode$rhs))
                        }
                        currentNum <- currentNum %/% 2   
                    }
                    if (length(clauses) > 0) {
                        rules <- rbind(rules, data.frame(
                            value = value,
                            clauses = I(list(clauses))
                        ))
                    }
                }
            }
            else
                val$leaf = FALSE
        }
        newTree[[as.numeric(trimws(data[[i]][1]))]] <- val
    }
    # Merge rules
    differentValues <- unique(rules$value)
    
    mergedRules <- data.frame(
                              controllerId = rep(command$controllerId, length(differentValues)),
                              accepted = rep(0, length(differentValues)),
                              clauses = I(rep(list(list()), length(differentValues)))
    )
    mergedRules$command <- data.frame(
        nodeId = rep(command$nodeId, length(differentValues)), 
        commandId = rep(command$id, length(differentValues)),
        value = rep(differentValues[1], length(differentValues)))
    for (i in 1:length(differentValues)) {
        newClauses <- list(I(rules[rules$value == differentValues[i],"clauses"]))
        mergedRules[i, "clauses"] <- I(list(newClauses))
        mergedRules$command[i, "value"] <- differentValues[i]
    }
    mergedRules
}
