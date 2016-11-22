# util.R
# Generic utility functions

util.extractUniqueId = function (name) {
    return(as.numeric(strsplit(name, "_")[[1]][2]))
}