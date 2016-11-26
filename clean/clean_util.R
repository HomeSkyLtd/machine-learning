# File for utility data cleaning and sampling functions
# All functions here must be generic. So they can be used in
# any kind of problem, not just in house automation.

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(lazyeval, quietly = TRUE, warn.conflicts = FALSE)
library(cluster, quietly = TRUE)


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

# Balance the data in input_table by undersampling the majority class represented
# by class_name. Undersampling is performed using CLARA, a  k-medoids clustering
# algorithm, sampling a few entries in each cluster
clean.clusterize_and_balance <- function(input_table) {
    # Get the counts of each class in the input table
    class_count <- table(input_table[, "action"])
    
    # Find the majority class
    maj_class <- names(which.max(class_count))
    #print(maj_class)
    #print(class_count)
    
    # Calculate the number of samples we want to have in the balanced dataset
    # Based on this number, calculate the number of clusters
    number_samples <- max(class_count[-which(names(class_count)==maj_class)])
    #print(number_samples)
    number_clusters <- min(3, floor(number_samples/5))
    #print(number_clusters)
    # Clusterize the data in the majority class
    # FIXME use filter_ to avoid hardcoding "0"
    table_to_sample <- filter(input_table, action == "NO_ACTION")

    clarax <- clara(table_to_sample, number_clusters,
                    correct.d = FALSE,
                    samples = 5
                    )
   # print(clarax)
    # sampled_table is the balanced dataset. Initialize it with the entries corresponding
    # to the minority classes
    # FIXME use filter_ to avoid hardcoding "0"
    sampled_table <- filter(input_table, action != "NO_ACTION")
    samples_per_cluster = ceiling(number_samples/number_clusters)
    
    # For each cluster, sample a few entries and add it to the final dataset
    #cat(paste("number of samples = ", number_samples, "\n"))
    for(i in 1:number_clusters) {
        rows_to_sample <- which(clarax$cluster == i)
        n_samples <- floor(number_samples * (length(rows_to_sample) / length(clarax$cluster)))
        cat("Getting ")
        cat(samples_per_cluster)
        cat(" samples (of ")
        cat(length(rows_to_sample))
        cat(" ) \n")
        print(clarax$medoids[i,])
        #if (n_samples > 1) {
            sampled_table <- rbind(sampled_table,
                mutate(select(data.frame(clarax$medoids[rep(i, samples_per_cluster),]), -action), action = "NO_ACTION")
           )
        #}
        #sampled_table <- 
        #    rbind(sampled_table, table_to_sample[
        #        sample(rows_to_sample, samples_per_cluster ), ]
        #   )
    }
    sampled_table
}



# Balance the data in input_table by undersampling the majority class represented
# by class_name. Undersampling is performed using CLARA, a  k-medoids clustering
# algorithm, sampling a few entries in each cluster
clean.clusterize_and_balance2 <- function(input_table) {
    print("Clusterizing")
    # Get the counts of each class in the input table
    class_count <- table(input_table[, "action"])
    
    # Find the majority class
    maj_class <- names(which.max(class_count))
   
    
    # Calculate the number of samples we want to have in the balanced dataset
    # Based on this number, calculate the number of clusters
    number_samples <- max(class_count[-which(names(class_count)==maj_class)])
    min_points_per_cluster <- max(class_count) / number_samples
    print(paste("min points per cluster", min_points_per_cluster))
    # Clusterize the data in the majority class
    # FIXME use filter_ to avoid hardcoding "0"
    table_to_sample <- filter(input_table, action == "NO_ACTION")
    max_luminance = max(table_to_sample$luminance)
    scan <- dbscan(select(mutate(table_to_sample, luminance = luminance / max_luminance), -action), eps = 2, minPts = 300)
    number_clusters <- max(scan$cluster)
    # sampled_table is the balanced dataset. Initialize it with the entries corresponding
    # to the minority classes
    # FIXME use filter_ to avoid hardcoding "0"
    sampled_table <- filter(input_table, action != "NO_ACTION")
    samples_per_cluster = ceiling(number_samples/number_clusters)
    
    # For each cluster, sample a few entries and add it to the final dataset
    cat(paste("samples_per_cluster ", samples_per_cluster, "\n"))
    print(paste("n clusters", number_clusters))
    for(i in 0:(number_clusters - 1)) {
        rows_to_sample <- which(scan$cluster == i)
        n_samples <- floor(number_samples * (length(rows_to_sample) / length(scan$cluster)))
        cat("Getting ")
        cat(n_samples)
        cat(" samples (of ")
        cat(length(rows_to_sample))
        cat(" ) \n")
        #print(clarax$medoids[i,])
        #if (n_samples > 1) {
        #   sampled_table <- rbind(sampled_table,
        #       mutate(select(data.frame(clarax$medoids[rep(i, samples_per_cluster),]), -action), action = "NO_ACTION")
        #  )
        #}
        sampled_table <- 
            rbind(sampled_table, table_to_sample[
                sample(rows_to_sample, samples_per_cluster ), ]
            )
    }
    sampled_table
}
