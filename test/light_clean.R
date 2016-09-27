# Script to clean the data in light 
#args <- commandArgs(trailingOnly = TRUE)
#if (length(args) != 1) {
#    stop("Usage: Rscript light_clean.R <INPUT_FILE>")
#}

#inputFile <- args[1]
#tbl <- read.table(inputFile)


source("../data_clean_sample_util.R")
    
tbl <- data.frame(light = c(0,0,0,50,50,49,0,0,0), presence = c(0,0,1,1,1,1,1,0,0), 
    light_on = c(0,0,0,1,1,2,0,0,0))

tbl <- clean.transform.edge(tbl, "light_on", "action")

tbl <- clean.smooth.subsequent(tbl, 'light', 3)

tbl

