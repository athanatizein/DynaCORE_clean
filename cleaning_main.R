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
data_en = read.csv("DynaCORE_test_answer_number.csv", sep = ",", stringsAsFactors = FALSE)
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



#################### plausibility checks & basic formatting ########################

data_en$language = as.factor(data_en$language)
data_en$Respondent.ID = as.factor(data_en$Respondent.ID )
data_en$Collector.ID = as.factor(data_en$Collector.ID)
data_en$older.or.18 = as.factor(data_en$older.or.18)
data_en$consent = as.factor(data_en$consent)
data_en$gender = as.factor(data_en$gender)
data_en$nationality = as.factor(data_en$nationality)
data_en$relationship.status = as.factor(data_en$relationship.status)
data_en$cohabitants = as.numeric(data_en$cohabitants)
data_en$cohabitants.underage = as.numeric(data_en$cohabitants.underage)
data_en$risk.group = as.factor(data_en$risk.group)
data_en$country.residence = as.factor(data_en$country.residence)
data_en$away.currently = as.factor(data_en$away.currently)
data_en$away.country = as.factor(data_en$away.country)
data_en$income = factor(data_en$income, order = TRUE)
data_en$illness.prone = factor(data_en$illness.prone, order = TRUE)
data_en$diagnosed.mental.health = as.factor(data_en$diagnosed.mental.health)
data_en$tested.pos = as.factor(data_en$tested.pos)
data_en$tested.pos.symptoms = factor(data_en$tested.pos.symptoms, order = TRUE)
data_en$quarantine = as.factor(data_en$quarantine)
data_en$C22_measures = as.numeric(data_en$C22_measures)
data_en$C23_compliance = as.numeric(data_en$C23_compliance)

## date and completion time ##

for(i in 1:length(data_en$Respondent.ID)){
  start = strsplit(data_en$Start.Date[i], " ")
  data_en$Start.Date[i] = start[[1]][1]
  data_en$Start.Time[i] = paste(start[[1]][2], start[[1]][3])
  
  end = strsplit(data_en$End.Date[i], " ")
  data_en$End.Date[i] = end[[1]][1]
  data_en$End.Time[i] = paste(end[[1]][2], end[[1]][3])
}

# put in proper date format - this is a bit tricky
#as.POSIXlt(data_en$Start.Date)
#as.numeric(now)
#[1] 1.262e+09
#R> now + 10  # adds 10 seconds
#[1] "2009-12-25 18:39:21 CST"


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


#### eduation #####

# extract the numeric component of free form education response
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# # test
# data_en$education[2] = "22 years"
# data_en$education[4] = "5 primary school 10 highschool"

data_en$education.fulltext = data_en$education
data_en$education = numextract(data_en$education)
data_en$education = as.numeric(data_en$education)

for(i in 1:length(data_en$Respondent.ID)){
  if (!is.na(data_en$education[i])) {
    if(data_en$education[i] > data_en$age[i]-3){
      data_en$education[i] = NA
    }
    if(!is.na(data_en$education[i]) && data_en$education[i] > 10){
      data_en$education.fulltext[i] = NA
    }
  }
}

# remove unnecessary columns 
xx = grep("X", colnames(data_en))
data_en = data_en[-xx]

################### restructure variables ########################

data_en[,c(68:154,156:167)] <- lapply(data_en[,c(68:154,156:167)], as.numeric)

#GHQ: 
term <- "CM"
GHQ <- grep(term, names(data_en))
GHQrecode <- function(x){recode(x, '1'=0L, '2'=1L, '3'=2L, '4'=3L)}
data_en[GHQ] <- lapply(data_en[GHQ], GHQrecode)

data_en$GHQsum <- rowSums(data_en[GHQ])

#SOZU:

term <- "H2_"
SOZU <- grep(term, names(data_en))
SOZU <- variables[1:7]

data_en$SOZUSum <- rowSums(data_en[SOZU])

#

# identify NAs, subjects to be excluded entirely..



##### quality control #####
# "education.fulltext" includes the full answer for education for anyone with less than 10 years.
# check these answers to make sure this was not due to typos or nor summing the total years of education

