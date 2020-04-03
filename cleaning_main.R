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
data_en = read.csv("DynaCORE_test_data_en.csv", sep = ",", stringsAsFactors = FALSE)
data_en$survey_country = as.factor("en")

rename(data_en)

# combine files from multiple languages

# data_xx = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xx$survey_country = as.factor("xx")
# 
# data_xy = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xy$survey_country = as.factor("xy")
# data_all = rbind(data_en, data_xx, data_xy)

################### general cleaning ########################

# remove first row and columns that are only NA
data_en = data_en[, colSums(is.na(data_en)) != nrow(data_en)]
data_en = data_en[-1,]
xx = which(!is.na(data_en$Respondent.ID))
# identify NAs, subjects to be excluded entirely..

#################### plausibility checks ########################




################### restructure variables ########################

