# pipeline template to clean and structure data from the DynaCORE project for analysis
# contributors:
# Matthias Zerban (matthias.zerban@unimedizin-mainz.de)
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

# VARIABLES 1-75: rename question columns and restructure format 
# needs to be hard coded to apply to all languages

data_ger = rename(data_ger)

# combine files from multiple languages

# data_xx = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xx$survey_country = as.factor("xx")
# 
# data_xy = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xy$survey_country = as.factor("xy")
# data_all = rbind(data_ger, data_xx, data_xy)


# remove first row and columns that are only NA
data_ger = data_ger[, colSums(is.na(data_ger)) != nrow(data_ger)]
data_ger = data_ger[-1,]
xx = which(!is.na(data_ger$Respondent.ID))
# identify NAs, subjects to be excluded entirely..

# compile variables of interest

