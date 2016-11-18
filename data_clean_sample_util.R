# File for utility data cleaning and sampling functions
# All functions here must be generic. So they can be used in
# any kind of problem, not just in house automation.

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(lazyeval, quietly = TRUE, warn.conflicts = FALSE)



# Apply a cleaning function to a data frame
clean.apply <- function(table, f, column, ..., new.column = column ) {
    table[[new.column]] <- f(table[[column]], ...)
    table
}

clean.smooth.subsequent <- function(v, n) {
    # Smooth each point with its subsequents
    # It is done from the begin to the end, so
    # the smooth doesn't spread over the role vector
    #
    # Args:
    #   v: the input vector, it must be 'summable'
    #   n: the number of subsequent elements to use in the smooth
    #
    # Returns:
    #   A new vector with the subsequent smooth applied
    
    sapply(seq_len(length(v)), function(i) {
        mean(v[i:min(length(v), i + n)])
    })
}

clean.smooth.precedent <- function(v, n) {
    # Smooth each point with its precedents
    # It is done from the end to the begin, so
    # the smooth doesn't spread over the role vector    
    # Args:
    #   v: the input vector, it must be 'summable'
    #   n: the number of precedent elements to use in the smooth
    #
    # Returns:
    #   A new vector with the precedent smooth applied
    
    sapply(seq_len(length(v)), function(i) {
        mean(v[max(1, i - n):i])
    })
}

clean.transform.timestamp <- function(v, date.info = "hour") {
    # Transforms a timestamp vector in a vector with 
    # a date info (like hour, day, ...)
    # 
    # Args:
    #   v: the input vector, it must contain timestamps
    #   date.info: string with the date info to extract. Can contain
    #              "hour" and "day". Its default is "hour".
    #
    # Returns:
    #   The vector with the desired info
    
    as.POSIXlt(v, origin = "1970-09-01")[[date.info]]
}

clean.transform.edge <- function(v) {
    # Extracts the edges (when there is change in the vector)
    #
    # Args:
    #   v: the vector to extract the edges, can be of any type,
    #      but must be a comparable type
    #
    # Retunrs:
    #   A new factor with the same length as v with NO_ACTION
    #   when there isn't action and <VALUE> when the next point
    #   assumes the value <VALUE>
    
    
    # Shift the input, to get the next action
    shiftedColumn <- c(tail(v, -1), NA)

    # Compare current value with next and create factors
    as.factor(
        ifelse(is.na(shiftedColumn) | is.na(v) |
            shiftedColumn == v, 'NO_ACTION', shiftedColumn))
}


sample.clusterize <- function(table) {

}