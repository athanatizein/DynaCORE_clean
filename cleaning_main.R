# pipeline template to clean and structure data from the DynaCORE project for analysis
# contributors:
# Matthias Zerban ()
# Kenneth Yuen ()
# Lara Puhlmann (puhlmann@cbs.mpg.de)

rm(list = ls())
require(foreign)
require(dplyr)
#require(plyr)
# source

# load data and add column indicating the origin of the data
data_ger = read.csv(".csv", sep = ",", stringsAsFactors = FALSE)
data_ger$survey = as.factor("ger")

data_xx = read.csv(".csv", sep = ",", stringsAsFactors = FALSE)
data_xx$survey = as.factor("xx")

data_xy = read.csv(".csv", sep = ",", stringsAsFactors = FALSE)
data_xy$survey = as.factor("xy")

data_all = merge(data_ger, data_xx, data_xy)

# combine files from multiple languages - this is for later

# identify NAs, subjects to be excluded entirely..

# compile variables of interest

# testing to see if this creates a new branch