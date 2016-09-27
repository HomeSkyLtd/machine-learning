# File for utility data cleaning and sampling functions
# All functions here must be generic. So they can be used in
# any kind of problem, not just in house automation.

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(lazyeval, quietly = TRUE, warn.conflicts = FALSE)

clean.smooth.subsequent <- function(table, column, n) {
    nrows <- length(table[[column]])
    table[['novo']] = 1:nrows
    #for (i in 1:nrows) {
    #    table[i, column] <- mean(table[i:min(c(i+n-1, nrows)), column]);
    #}
    table[[column]] <- mean(table[[ table[[column]]  ]])
    table;
}

clean.smooth.precedent <- function(table, column, n) {
    nrows <- length(table[column][[1]]);
    table[['novo']] = 1:nrows
    for (i in nrows:1) {
        table[i, 'novo'] <- mean(table[max(i-n+1, 1):i, column])
    }
    table;
}

clean.transform.timestamp <- function(table, timestamp.column, date.columns = c("hour")) {

}

clean.transform.edge <- function(table, column, new.column) {
    # Extracts the edges from the column, which becomes a FACTOR
    # Factors are named after the respective new values
    # When there is no change in value, the factor NO_CHANGE is used.

    # Creates new column with next state of column
    table[[new.column]] <- c(tail(table[[column]], -1), NA)

    # Compare current value with next and create factors
    table[[new.column]] <- as.factor(
        ifelse(is.na(table[[new.column]]) | is.na(table[[column]]) |
        table[[new.column]] == table[[column]],
        'NO_CHANGE', table[[new.column]]))
    
    # Returns table
    table
}


sample.clusterize <- function(table) {

}