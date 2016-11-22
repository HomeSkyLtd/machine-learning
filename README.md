#Machine Learning for home automation

This repository contains R scripts to create rules for a house. 

##Code

All code uses dplyr data frames.
The naming convention is to use lowercase names with dots for functions and function arguments.
For variables, Snake case is used.

###generator.R
This file contains one function (house.generate.rules) that integrates all other files in order to learn rules.

###db.R
This file contains functions to connect, save and load data from the database.

###clean/clean_util.R
This file contains utility functions to clean the data: smoothing, sampling, clustering, timestamp transformation.

###clean/lightswitch.R
Specific cleaning for light problem.

###train.R
This file actually does the learning and interpretation of learning algorithm output

###util.R
Generic utility functions, like extracting uniqueId from a data.frame row

###run.R
Start rServe that can receive calls from the clojure server

##Dependencies
In this project we used the following R libraries:
- mongolite
- jsonlite
- dplyr
- rpart
- lazyeval

##How to run
1. Make sure you have the database with data and nodes (either using the house_preparing project or running a server instance)
2. Start R
3. Make sure you install all the dependencies in your R environment with ```install.packages```
4. Set your working directory to the root folder from this project with ```setwd```
5. Execute: ```source("./generator.R")``` to load the generator file
6. Now you can execute the house.generate.rules function, passing the houseId

