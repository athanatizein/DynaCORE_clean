# pipeline template to clean and structure data from the DynaCORE project for analysis
# contributors:
# Matthias Zerban (matthias.zerban@unimedizin-mainz.de)
# Kenneth Yuen ()
# Lara Puhlmann (puhlmann@cbs.mpg.de)
# Jeroen Weermeijer (jeroen.weermeijer@kuleuven.be)




#
#WARNING: Only run once! 
#


if (!require("pacman")) install.packages("pacman")

pacman::p_load(plyr,dplyr,foreign, stringr, BBmisc, stringr)


rm(list = ls())
# require(foreign)
# require(plyr)
# require(dplyr)
# require(stringr)
# require(BBmisc)
# require(stringr)
# run the functions 'rename.R', 'formatting.R', ... or source:
source("rename.R")
source("formatting.R")

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


#################### covariates: plausibility checks & basic formatting ########################
data_en[,c(1:2, 10:12,14:16, 18:19, 53:54, 58:59, 60:61,64)] <- lapply(data_en[,c(1:2, 10:12,14:16, 18:19, 53:54, 58:59, 60:61,64)], as.factor)

data_en$household.income = factor(data_en$household.income, order = TRUE)
data_en$health.status = factor(data_en$health.status, order = TRUE)
data_en$symptom.severity = factor(data_en$symptom.severity, order = TRUE)

data_en$people.in.household.under.18 = as.numeric(data_en$people.in.household.under.18)
data_en$opinion.about.authorities.measures = as.numeric(data_en$opinion.about.authorities.measures)
data_en$adherence.to.recommended.procedures = as.numeric(data_en$adherence.to.recommended.procedures)

##### people.in.household as continuous ####
data_en$people.in.household.cont = as.numeric(data_en$people.in.household)
data_en$people.in.household.cont[which(data_en$people.in.household.cont == 3)] = 3.5
data_en$people.in.household.cont[which(data_en$people.in.household.cont == 4)] = 5.5
xx = numextract(data_en$X.28)
data_en$people.in.household.cont[which(data_en$people.in.household.cont == 5)] = as.numeric(xx)

###### date and completion time ########

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

#convert month-day-year to year-month-day date, then to POSIXlt
data_en$Start.Date = as.Date(data_en$Start.Date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
data_en$End.Date = as.Date(data_en$End.Date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
data_en$Start.Date = as.POSIXlt(paste(data_en$Start.Date), tz = "Europe/Berlin", format="%Y-%m-%d")
data_en$End.Date = as.POSIXlt(paste(data_en$End.Date), tz = "Europe/Berlin", format="%Y-%m-%d")
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


###### date of Corona test #####
data_en$infection.test.status.date = gsub(".", "/", data_en$infection.test.status.date, fixed=TRUE)#mm.dd.yyyy becomes mm/dd/yyyy
data_en$infection.test.status.date[which(nchar(data_en$infection.test.status.date)<5)]=NA # set all that are not a date to NA
#convert month-day-year to year-month-day date
data_en$infection.test.status.date = as.Date(data_en$infection.test.status.date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
#date as POSIXlt
data_en$infection.test.status.date = as.POSIXlt(paste(data_en$infection.test.status.date), tz = "Europe/Berlin", format="%Y-%m-%d")


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

###### education #####

# # test
# data_en$years.of.education[2] = "22 years"
# data_en$years.of.education[4] = "5 primary school 10 highschool"

data_en$years.of.education.fulltext = data_en$years.of.education
data_en$years.of.education = numextract(data_en$years.of.education) # extract the numeric component of free form years.of.education response
data_en$years.of.education = as.numeric(data_en$years.of.education)

for(i in 1:length(data_en$Respondent.ID)){
  if (!is.na(data_en$years.of.education[i])) {
    if(data_en$years.of.education[i] > data_en$age[i]){
      data_en$years.of.education[i] = NA
    }
    if(!is.na(data_en$years.of.education[i]) && data_en$years.of.education[i] > 10){
      data_en$years.of.education.fulltext[i] = NA
    }
  }
}

###### current.location ####
## combines the variables "country.of.residence", "current.stay.out.of.town.country" and "currently.away" into a variable "current.location" -> if subjects are away, their their location is the away country location, if they are not, the country of residence location is
##R can default character columns to factors, so first step is to make sure variables of interest are characters
data_en[ , c("current.stay.out.of.town.country" ,"country.of.residence") ] <- sapply( data_en[ , c("current.stay.out.of.town.country" ,"country.of.residence") ] , as.character )
##now we use a simple ifelse statement to create a new variable that gives us currenty location (country)
data_en$current.location <- ifelse(data_en$current.stay.out.of.town == '1', data_en$current.stay.out.of.town.country, data_en$country.of.residence)

# #test
# data_en$country.of.residence[2] #gives Algeria
# data_en$current.stay.out.of.town.country[2] #Gives Andorra
# data_en$current.stay.out.of.town[2] #Gives Yes, so current loc should be Andorra
# data_en$current.location[2] #gives Andorra, hooray            

#### clean-up inconsistent responses ####
# set responses for place of location to NA if away currently was answered with 0
data_en$current.stay.out.of.town.country[which(data_en$current.stay.out.of.town==2)] <- NA
data_en$current.stay.out.of.town.city[which(data_en$current.stay.out.of.town==2)] <- NA

# set cases where more/same people in household are underage than total household to NA
xx = which(data_en$people.in.household.cont+0.5<=data_en$people.in.household.under.18)
data_en$people.in.household[xx] = NA
data_en$people.in.household.cont[xx] = NA
data_en$people.in.household.under.18[xx] = NA

# set cases with mismatch in occulation to NA
data_en$not.working.12 <- lapply(data_en$occupation, function(ch) grep("16", ch))
data_en$not.working.12[sapply(data_en$not.working.12, function(x) length(x)==0)] <- NA

### Employment
employed = c("1", "2", "3", "4")
data_en$employed.13 <- lapply(data_en$occupational.status, function(ch) grep(paste(employed, collapse="|"), ch))
data_en$employed.13[sapply(data_en$employed.13, function(x) length(x)==0)] <- NA

# find people that are unemployed in 12 but working in 13
xx = which(!is.na(data_en$not.working.12) && !is.na(data_en$employed.13))
data_en$occupation[xx] <- NA
data_en$occupational.status[xx] <- NA

# find people that are employed in 12 but not working in 13
# STILL to do

# I think mental health is wrongly coded as 0 = yes, 1 = no, so recode ONCE ONLY:
data_en$diagnosed.mental.health = revalue(data_en$diagnosed.mental.health, c("0"="1", "1"="0"))



################### restructure questionnaire variables ########################

data_en[,c(68:154,156:167)] <- lapply(data_en[,c(68:154,156:167)], as.numeric)

#get rid of any cases with missings:

data_en$missing <- rowSums(is.na(data_en[,c(68:154,156:167)]))
data_en <- data_en[data_en$missing == 0,]

#Mental Health Problems 'P': 
data_en$CM_07 <- 5 - data_en$CM_07
term <- "CM"
GHQ <- grep(term, names(data_en))
GHQrecode <- function(x){recode(x, '1'=0L, '2'=1L, '3'=2L, '4'=3L)}
data_en[GHQ] <- lapply(data_en[GHQ], GHQrecode)
GHQ <- GHQ[1:12]
data_en$P <- rowSums(data_en[GHQ])

#PSS (percieved social support):
term <- "H2_"
PSSindex <- grep(term, names(data_en))
#PSSindex <- SOZU[1:7]
data_en$PSS <- rowSums(data_en[PSSindex])

#COVID-19 support:
data_en$CSS <- as.numeric(data_en$H2_08)

#Optimism:
data_en$OPT <- as.numeric(data_en$H3_01)

#Perceived general self efficacy:
term <-"H4_"
GSEindex <- grep(term, names(data_en))
data_en$GSE <- rowSums(data_en[GSEindex])

# self-percieved reslience (BRS):
term <- "H5_"
BRS <- grep(term, names(data_en))
BRSrec <- c("H5_02", "H5_04", "H5_06")
data_en[,BRSrec] <- 6 - data_en[,BRSrec]
data_en$REC <- rowMeans(data_en[BRS])

#BFI Neuroticism:
data_en$H6_01 <- 6 - data_en$H6_01
term <- "H6_"
BFI <- grep(term, names(data_en))
BFIrecode <- function(x){recode(x, '1'=-2L, '2'=-1L, '3'=0L, '4'=1L,'5'=2L)}
data_en[BFI] <- lapply(data_en[BFI], BFIrecode)
data_en$NEU <- rowSums(data_en[BFI])

#Behavioral Coping style
term <- "COPE"
COPE <- grep(term, names(data_en))
data_en$BCS <- rowSums(data_en[COPE])

#CERQ
term <- "CERQ"
CERQ <- grep(term, names(data_en))
data_en$CERQSum <- rowSums(data_en[CERQ])

#Positive Appraisal Style:
PAS <- data_en[,c(CERQ, 106, 110 )]

#HE: According to prereg we just do Z scores, but this aproach might be better. 
PAS[,c("H1_COPE_18","H1_COPE_28")] <- PAS[,c("H1_COPE_18","H1_COPE_28")]*5/4 #rescale

data_en$PAS <- rowMeans(PAS)

#CORONA specific appraisal:
term <- "H1_Cor_"
PAC <- grep(term, names(data_en))
data_en$PAC <- rowSums(data_en[PAC])

#### calculation of stressors
# SCM = stressor count method
# SSM = stressor severity method

#CORONA Stressors:
term <- "CE_"
CE <- grep(term, names(data_en))
CE <- CE[1:30]
data_en$Es.SCM <- rowSums(data_en[CE] >0) #stressor count
data_en$Es.SSM <- rowSums(data_en[CE])/5 #weighted

#DHs:
term <- "GE_"
GE <- grep(term, names(data_en))
GE <- GE[1:12]
data_en$Eg.SCM <- rowSums(data_en[GE] >0) #stressor count
data_en$Eg.SSM <- rowSums(data_en[GE])/5 #weighted

#combined:
Eall <- c(grep("GE_", names(data_en))[1:12], grep("CE_", names(data_en))[1:30])
data_en$Ec.SCM <- rowSums(data_en[Eall] >0) #stressor count
data_en$Ec.SSM <- rowSums(data_en[Eall])/5 #weighted

# test
which(data_en$Ec.SCM!=rowSums(data_en[ , c("Eg.SCM" ,"Es.SCM")]))
#--> should be 0

##################### SR Score ###################

#adapted from Haakon's script
m1 <- summary(lm(scale(P)~scale(Eg.SCM),data= data_en))
data_en$SR_Eg.SCM <-as.numeric(scale(resid(m1)))

m2 <- summary(lm(scale(P)~scale(Es.SCM),data= data_en))
data_en$SR_Es.SCM <-as.numeric(scale(resid(m2)))

m3 <- summary(lm(scale(P)~scale(Ec.SCM),data= data_en))
data_en$SR_c.SCM <-as.numeric(scale(resid(m3)))

## do the same with severity ratings?
m4 <- summary(lm(scale(P)~scale(Eg.SSM),data= data_en))
data_en$SR_Eg.SSM <-as.numeric(scale(resid(m4)))

m5 <- summary(lm(scale(P)~scale(Es.SSM),data= data_en))
data_en$SR_Es.SSM <-as.numeric(scale(resid(m5)))

m6 <- summary(lm(scale(P)~scale(Ec.SSM),data= data_en))
data_en$SR_c.SSM <-as.numeric(scale(resid(m6)))
######################## subgroup selection ######################## 

##### select subjects FROM Europe
Europe = c(2, 4, 9, 11, 12, 17, 18, 23, 28, 45, 47, 48, 51, 60, 63, 64, 67, 68, 70, 77, 80, 81, 86, 88, 98, 103, 104, 105, 111, 117, 119, 127, 132, 142, 143, 146, 147, 154, 158, 162, 163, 168, 174, 175, 180, 191, 193)
# for now, the above list does not include Russia (148), Kasakhstan (92) & Turkey (186), since they are trans-continental

xx = which(data_en$country.of.residence %in% Europe)
data_en$from.eu = 0
data_en$from.eu[xx] = 1
data_en$from.eu = as.factor(data_en$from.eu)

##### select subjects IN Europe
xx = which(data_en$current.location %in% Europe)
data_en$in.eu = 0
data_en$in.eu[xx] = 1
data_en$in.eu = as.factor(data_en$in.eu)

##################### identify subjects to exclude ##############
# exclude subjects under 18
data_en$Respondent.ID[which(data_en$age < 18)]<- NA

# exclude subjects with very short completion time
# data_en$Respondent.ID[which(data_en$completionTime < 400)]<- NA

Europe = c(2, 4, 9, 11, 12, 17, 18, 23, 28, 45, 47, 48, 51, 60, 63, 64, 67, 68, 70, 77, 80, 81, 86, 88, 98, 103, 104, 105, 111, 117, 119, 127, 132, 142, 143, 146, 147, 154, 158, 162, 163, 168, 174, 175, 180, 191, 193)
# for now, the above list does not include Russia (148), Kasakhstan (92) & Turkey (186), since they are trans-continental

##### select subjects FROM Europe
#xx = which(data_en$country.of.residence %in% Europe)

##### select subjects IN Europe
#xx = which(data_en$current.location %in% Europe)

data_eu = data_en[xx,]


### exclude subjects with no response variance (check block-wise)
var = matrix(NA, nrow = length(data_en$Respondent.ID), ncol = 8)
for (i in 1:nrow(data_en)){ 
  var[i,1] = (var(as.vector(as.matrix(data_en[i, GHQ])))) 
  var[i,2] = (var(as.vector(as.matrix(data_en[i, SOZU])))) 
  var[i,3] = (var(as.vector(as.matrix(data_en[i, ASKU])))) 
  var[i,4] = (var(as.vector(as.matrix(data_en[i, BRS])))) 
  var[i,5] = (var(as.vector(as.matrix(data_en[i, COPE])))) 
  var[i,6] = (var(as.vector(as.matrix(data_en[i, CERQ])))) 
  var[i,7] = (var(as.vector(as.matrix(data_en[i, CE])))) 
}

data_en$response_variance = rowSums(var)
#add variance as an additional column in the data frame
#data_en$variance_Q1 <- apply(data_en,1,function(row) var(as.vector(row[5:10]))) #change 5:10 
 
data_en$Respondent.ID[which(data_en$response_variance == 0)]<- NA

###Comment: if the columns are not neatly lined up (e.g. following the 5:10 example) you can also use a vector. In it, specify column names, example below
# x = c("colname1", "colname2", "colname3", "colname4", "colname5", "colname6") #specify columns per questionnaire
# for (i in 1:nrow(data_en)){ 
#   print(var(as.vector(as.matrix(data_en[i, x])))) #x reflects columns used, change accordingly per questionnaire (i.e. change with another vector) 
# }

# #add variance as an additional column in the data frame
# data_en$variance_Q1 <- apply(data_en,1,function(row) var(as.vector(row[x]))) 

### exclude subjects based on completion time?
#data_en$Respondent.ID[which(data_en$completionTime < 400)]<- NA

xx = which(is.na(data_en$Respondent.ID))
data_en = data_en[-xx,]

# remove unnecessary columns 
xx = grep("X", colnames(data_en))
data_en = data_en[-xx]

data_en = data_en[, colSums(is.na(data_en)) != nrow(data_en)]


######### processing covariates ############

# index people who work in healthcare
# index people who are unemployed

# index people who work as freelancers

# list of mental health conditions
                           


##### quality control #####

# "years.of.education.fulltext" includes the full answer for years.of.education for anyone with less than 10 years
# check these answers to make sure this was not due to typos or nor summing the total years of 

