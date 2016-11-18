# File for training data using a tree-based method, returning a tree
# All functions here must be generic. So they can be used in
# any kind of problem, not just in house automation.


library(rpart)
library(rpart.plot)

train.tree = function (data, y) {
    # Function that trains a tree from data
    # 
    # Args:
    #   data: inptu data. It is a dataframe
    #   y: The column that we want to predict based on the other
    #      columns
    # Returns:
    #   Trained tree
    
    trained <- do.call("rpart", list(as.formula(paste(y, "~", ".")), data = data))
    rpart.plot(trained, type = 4)
    
}