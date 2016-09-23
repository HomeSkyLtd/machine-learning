# File for utility data cleaning and sampling functions
# All functions here must be generic. So they can be used in
# any kind of problem, not just in house automation.


clean.smooth.subsequent <- function(table, column, n) {
    nrows <- length(table[column][[1]]);
    for (i in 1:nrows) {
        table[i, column] <- mean(table[i:min(c(i+n-1, nrows)), column][[1]]);
    }
    table;
}

clean.smooth.precedent <- function(table, column, n) {
    nrows <- length(table[column][[1]]);
    for (i in nrows:1) {
        table[i, column] <- mean(table[max(i-n+1, 1):i, column][[1]]);
    }
    table;
}

clean.transform.timestamp <- function(table, timestamp.column, date.columns = c("hour")) {

}

clean.transform.edge <- function(table, column) {
    # Extracts the edges from the column, which becomes a FACTOR
    # Factors are named EDGE_FROM_TO, where FROM and TO are replaced
    # with the respective values. When there is no change in value,
    # the factor NO_CHANGE is used.
}


sample.clusterize <- function(table) {

}