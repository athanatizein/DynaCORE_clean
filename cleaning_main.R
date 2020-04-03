# pipeline template to clean and structure data from the DynaCORE project for analysis
# contributors:
# Matthias Zerban (matthias.zerban@unimedizin-mainz.de)
# Kenneth Yuen ()
# Lara Puhlmann (puhlmann@cbs.mpg.de)

rm(list = ls())
require(foreign)
require(dplyr)
require(stringr)
#require(plyr)
# run the functions 'rename.R', 'formatting.R', ... or source:
# source("/.../DynaCORE_clean/rename.R")
# source("/.../DynaCORE_clean/formatting.R")

# load data and add column indicating the origin of the data
data_en = read.csv("DynaCORE_test_data_en.csv", sep = ",", stringsAsFactors = FALSE)
data_en$survey_country = as.factor("en")

data_en = rename(data_en)

########## combine files from multiple languages

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

# note that in the real data, the above step will also exclude the column IP address



#################### plausibility checks ########################

#### age #####

# extract the numeric component of free form age response
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# # test
# data_en$age[2] = "22 years old"
# data_en$age[4] = "I am 711 years old"

data_en$age = numextract(data_en$age)
data_en$age = as.numeric(data_en$age)

for(i in 1:length(data_en$Respondent.ID)){
  if (!is.na(data_en$age[i])) {
    if(data_en$age[i] < 18 || data_en$age[i] > 100) {
      data_en$age[i] = NA
    }
  }
}

################### restructure variables ########################


# identify NAs, subjects to be excluded entirely..