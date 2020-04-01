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
data_ger = read.csv("DynaCORE_test_data_ger.csv", sep = ",", stringsAsFactors = FALSE)
data_ger$survey_country = as.factor("ger")

data_xx = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
data_xx$survey_country = as.factor("xx")

data_xy = read.csv(".csv", sep = ",", stringsAsFactors = FALSE)
data_xy$survey_country = as.factor("xy")

# combine files from multiple languages - this is for later
data_all = merge(data_ger, data_xx, data_xy)
xx = which(!is.na(data_ger$Respondent.ID))
# identify NAs, subjects to be excluded entirely..

# compile variables of interest

# testing to see if this creates a new branch