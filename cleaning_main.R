# pipeline template to clean and structure data from the DynaCORE project for analysis
# contributors:
# Matthias Zerban (matthias.zerban@unimedizin-mainz.de)
# Kenneth Yuen ()
# Lara Puhlmann (puhlmann@cbs.mpg.de)
# Jeroen Weermeijer (jeroen.weermeijer@kuleuven.be)




#
#WARNING: Only run once! 
#



rm(list = ls())
require(foreign)
require(dplyr)
require(stringr)
require(BBmisc)
require(stringr)
#require(plyr)
# run the functions 'rename.R', 'formatting.R', ... or source:
# source("/.../DynaCORE_clean/rename.R")
# source("/.../DynaCORE_clean/formatting.R")

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# load data and add column indicating the origin of the data
data_en = read.csv("DynaCORE_test_answer_number.csv", sep = ",", stringsAsFactors = FALSE)
#data_test = read.csv("DynaCORE_test_data_firstround.csv", sep = ",", stringsAsFactors = FALSE)

data_en$survey_country = as.factor("en")

data_en = rename(data_en)
data_en = formatting(data_en) #group occupation + status in lists

################### general cleaning ########################

# remove rows without respondent ID
xx = which(is.na(data_en$Respondent.ID))
data_en = data_en[-xx,]
# note that in the real data, the above step will also exclude the column IP address

########## combine files from multiple languages

# data_xx = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xx$survey_country = as.factor("xx")
# 
# data_xy = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xy$survey_country = as.factor("xy")
# data_all = rbind(data_en, data_xx, data_xy)


#################### plausibility checks & basic formatting ########################
data_en[,c(1:2, 10:12,14:16, 18:19, 53:54, 58:59, 60:61,64)] <- lapply(data_en[,c(1:2, 10:12,14:16, 18:19, 53:54, 58:59, 60:61,64)], as.factor)

data_en$income = factor(data_en$income, order = TRUE)
data_en$illness.prone = factor(data_en$illness.prone, order = TRUE)
data_en$tested.pos.symptoms = factor(data_en$tested.pos.symptoms, order = TRUE)

data_en$cohabitants.underage = as.numeric(data_en$cohabitants.underage)
data_en$C22_measures = as.numeric(data_en$C22_measures)
data_en$C23_compliance = as.numeric(data_en$C23_compliance)

data_en$cohabs.cont = as.numeric(data_en$cohabitants)
data_en$cohabs.cont[which(data_en$cohabs.cont == 3)] = 3.5
data_en$cohabs.cont[which(data_en$cohabs.cont == 4)] = 5.5
xx = numextract(data_en$X.28)
data_en$cohabs.cont[which(data_en$cohabs.cont == 5)] = as.numeric(xx)

# I think mental health is wrongly coded as 0 = yes, 1 = no, so recode ONCE ONLY:
data_en$diagnosed.mental.health = revalue(data_en$diagnosed.mental.health, c("0"="1", "1"="0"))

################# date and completion time ########

#split weird month-day-year date + time col into two col, one with the weird date format, one with correct time
for(i in 1:length(data_en$Respondent.ID)){
  start = strsplit(data_en$Start.Date[i], " ")
  data_en$Start.Date[i] = start[[1]][1]
  data_en$Start.Time[i] = paste(start[[1]][2], start[[1]][3])
  
  end = strsplit(data_en$End.Date[i], " ")
  data_en$End.Date[i] = end[[1]][1]
  data_en$End.Time[i] = paste(end[[1]][2], end[[1]][3])
}

#line of code that deals with different separators that occur in surveymonkey raw data for date outputs (e.g. mm/dd/yyyy vs. mm.dd.yyyy).
data_en$Start.Date = gsub(".", "/", data_en$Start.Date, fixed=TRUE) #mm.dd.yyyy becomes mm/dd/yyyy

#convert month-day-year to year-month-day date
data_en$Start.Date = as.Date(data_en$Start.Date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
data_en$End.Date = as.Date(data_en$End.Date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
#date as POSIXlt
data_en$Start.Date = as.POSIXlt(paste(data_en$Start.Date), tz = "Europe/Berlin", format="%Y-%m-%d")
data_en$End.Date = as.POSIXlt(paste(data_en$End.Date), tz = "Europe/Berlin", format="%Y-%m-%d")
#date+time as POSIXlt
data_en$Start.DateTime = as.POSIXlt(paste(data_en$Start.Date, data_en$Start.Time), tz = "Europe/Berlin", format="%Y-%m-%d %H:%M:%S %p")
data_en$End.DateTime = as.POSIXlt(paste(data_en$End.Date, data_en$End.Time), tz = "Europe/Berlin", format="%Y-%m-%d %H:%M:%S %p")
# #test
# data_en$Start.Date[4] #should give year/month/day GMT
# data_en$End.Date[4] #should give year/month/day GMT
# data_en$Start.DateTime[4] #should give year/month/day hour/minutes/seconds GMT
# data_en$End.DateTime[4] #should give year/month/day hour/minutes/seconds GMT

#completion time
data_en$completionTime = difftime(data_en$End.DateTime, data_en$Start.DateTime) 
# #test
# data_en$completionTime[3] #gives time difference in minutes


##### date of Corona test #####
data_en$tested.pos.date = gsub(".", "/", data_en$tested.pos.date, fixed=TRUE)#mm.dd.yyyy becomes mm/dd/yyyy
data_en$tested.pos.date[which(nchar(data_en$tested.pos.date)<5)]=NA # set all that are not a date to NA
#convert month-day-year to year-month-day date
data_en$tested.pos.date = as.Date(data_en$tested.pos.date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
#date as POSIXlt
data_en$tested.pos.date = as.POSIXlt(paste(data_en$tested.pos.date), tz = "Europe/Berlin", format="%Y-%m-%d")


###### age #####
# # test
# data_en$age[2] = "2o"
# data_en$age[4] = "I am 711 years old"

data_en$age = gsub("o", "0", data_en$age)
# if any ages are 0, this could be due to a leading o
 
# extract the numeric component of free form age response
data_en$age = numextract(data_en$age)
data_en$age = as.numeric(data_en$age)
data_en$age[which(data_en$age > 100)] = NA

#### education #####

# # test
# data_en$education[2] = "22 years"
# data_en$education[4] = "5 primary school 10 highschool"

data_en$education.fulltext = data_en$education
data_en$education = numextract(data_en$education) # extract the numeric component of free form education response
data_en$education = as.numeric(data_en$education)

for(i in 1:length(data_en$Respondent.ID)){
  if (!is.na(data_en$education[i])) {
    if(data_en$education[i] > data_en$age[i]){
      data_en$education[i] = NA
    }
    if(!is.na(data_en$education[i]) && data_en$education[i] > 10){
      data_en$education.fulltext[i] = NA
    }
  }
}

################### restructure variables ########################

data_en[,c(68:154,156:167)] <- lapply(data_en[,c(68:154,156:167)], as.numeric)

#GHQ: 
term <- "CM"
GHQ <- grep(term, names(data_en))
GHQrecode <- function(x){recode(x, '1'=0L, '2'=1L, '3'=2L, '4'=3L)}
data_en[GHQ] <- lapply(data_en[GHQ], GHQrecode)
data_en$GHQsum <- rowSums(data_en[GHQ])

#SOZU (percieved soc support):

term <- "H2_"
SOZU <- grep(term, names(data_en))
SOZU <- variables[1:7]
data_en$SOZUSum <- rowSums(data_en[SOZU])

#COVID-19 support:
#data_en$H2_08 <- as.numeric(data_en$H2_08)

#Optimism:
#data_en$H3_01 <- as.numeric(data_en$H3_01)

#ASKU (self efficacy):
term <-"H4_"
ASKU <- grep(term, names(data_en))
data_en$ASKUSum <- rowSums(data_en[ASKU])

# self-percieved reslience (BRS):
term <- "H5_"
BRS <- grep(term, names(data_en))
BRSrec <- c("H5_02", "H5_04", "H5_06")
data_en[,BRSrec] <- 6 - data_en[,BRSrec]
data_en$BRSMean <- rowMeans(data_en[BRS])

#BFI Neuroticism:
data_en$H6_01 <- 6 - data_en$H6_01
term <- "H6_"
BFI <- grep(term, names(data_en))
BFIrecode <- function(x){recode(x, '1'=-2L, '2'=-1L, '3'=0L, '4'=1L,'5'=2L)}
data_en[BFI] <- lapply(data_en[BFI], BFIrecode)
data_en$BFIsum <- rowSums(data_en[BFI])

#COPE
term <- "COPE"
COPE <- grep(term, names(data_en))
data_en$COPESum <- rowSums(data_en[COPE])

#CERQ
term <- "CERQ"
CERQ <- grep(term, names(data_en))
data_en$CERQSum <- rowSums(data_en[CERQ])

#Positive Appraisal Style:

PAS <- data_en[,c(CERQ, 106, 110 )]
PAS[,c("H1_COPE_18","H1_COPE_28")] <- PAS[,c("H1_COPE_18","H1_COPE_28")]*5/4 #rescale
data_en$PASMean <- rowMeans(PAS)

#CORONA Stressors:
term <- "CE_"
CE <- grep(term, names(data_en))
CE <- CE[1:30]
data_en$CEcount <- rowSums(data_en[CE] >0) #stressor count

# I will procede working here later/MZ



##################### identify subjects to exclude ##############
# exclude subjects under 18
data_en$Respondent.ID[which(data_en$age < 18)]<- NA

# exclude subjects with very short completion time
data_en$Respondent.ID[which(data_en$completionTime < 4)]<- NA

# exclude subjects with no response variance (check block wise)

# calculate variance
var(as.vector(as.matrix(data[1, 5:10]))) #calculates variance for row 1, column 5:10
variance = lapply(data_en[GHQ],var())

#------------------------------Suggestion: calculating variance per questionnaire--------------------------
for (i in 1:nrow(data_en)){ 
  print(var(as.vector(as.matrix(data_subset[i, 5:10])))) #5:10 reflects column 5 to 10, change accordingly per questionnaire 
}
#add variance as an additional column in the data frame
data_en$variance_Q1 <- apply(data_en,1,function(row) var(as.vector(row[5:10]))) #change 5:10 
                          
###Comment: if the columns are not neatly lined up (e.g. following the 5:10 example) you can also use a vector. In it, specify column names, example below
# x = c("colname1", "colname2", "colname3", "colname4", "colname5", "colname6") #specify columns per questionnaire
# for (i in 1:nrow(data_en)){ 
#   print(var(as.vector(as.matrix(data_subset[i, x])))) #x reflects columns used, change accordingly per questionnaire (i.e. change with another vector) 
# }

# #add variance as an additional column in the data frame
# data_en$variance_Q1 <- apply(data_en,1,function(row) var(as.vector(row[x]))) 
                    
                          
#------------------------------Suggestion end--------------------------

variance = lapply(data_en[SOZU],var())
#ASKU
#BRS
#BFI
#..

data_en$Respondent.ID[which(data_en$completionTime < 18)]<- NA

xx = which(is.na(data_en$Respondent.ID))
data_en = data_en[-xx,]

#xx = which(!is.na(data_en$Respondent.ID))


# remove unnecessary columns 
xx = grep("X", colnames(data_en))
data_en = data_en[-xx]

data_en = data_en[, colSums(is.na(data_en)) != nrow(data_en)]

##### quality control #####

# "education.fulltext" includes the full answer for education for anyone with less than 10 years.
# check these answers to make sure this was not due to typos or nor summing the total years of education

