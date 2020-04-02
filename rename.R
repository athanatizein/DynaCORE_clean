rename <- function(df){
  
  names(df)[1] <- "Respondent.ID"
  names(df)[2] <- "Collector.ID"
  names(df)[3] <- "Start.Date"
  names(df)[4] <- "End.Date"
  names(df)[5] <- "IP.Address"
  names(df)[10] <- "language"
  names(df)[11] <- "older.or.18"
  names(df)[12] <- "consent"
  names(df)[13] <- "age"
  names(df)[14] <- "gender"
  names(df)[15] <- "nationality"
  names(df)[16] <- "country.residence"
  names(df)[17] <- "city.residence"
  names(df)[18] <- "away.currently"
  names(df)[19] <- "away.country"
  names(df)[20] <- "away.city"
  names(df)[21] <- "education"
  names(df)[22] <- "occupation"
  names(df)[39] <- "occupational.status"
  names(df)[52] <- "income"
  names(df)[53] <- "relationship.status"
  names(df)[54] <- "cohabitants"
  names(df)[56] <- "cohabitants.underage"
  names(df)[57] <- "illness.prone"
  names(df)[58] <- "diagnosed.mental.health"
  names(df)[60] <- "risk.group"
  names(df)[61] <- "tested.pos"
  names(df)[63] <- "tested.pos.symptoms"
  names(df)[64] <- "quarantine"
  names(df)[66] <- "C22_measure2"
  names(df)[67] <- "C23_compliance"
  names(df)[68] <- "CM_01"
  names(df)[69] <- "CM_02"
  names(df)[70] <- "CM_03"
  names(df)[71] <- "CM_04"
  names(df)[72] <- "CM_05"
  names(df)[73] <- "CM_06"
  names(df)[74] <- "CM_07"
  names(df)[75] <- "CM_08"
  names(df)[76] <- "CM_09"
  names(df)[77] <- "CM_10"
  names(df)[78] <- "CM_11"
  names(df)[79] <- "CM_12"
  names(df)[80] <- "CM_13"
  
  
# While it is possible to use grepl to match the strings in multiple choice questions, I was not sure if the text will be different depending on the language of the survey. So I tried a workaround based on position in the survey only - LP
  
#x <- vector(mode = "list", length = 0)
for(i in 1:length(df$Respondent.ID)){
  x <- vector(mode = "list", length = 0)
  if(nchar(df$X.1[i]) > 0){
    x[[length(x)+1]] = 1
  } 
  if(nchar(df$X.2[i]) > 0){
    x[[length(x)+1]] = 2
  } 
  if(nchar(df$X.3[i]) > 0){
    x[[length(x)+1]] = 3
  } 
  if(nchar(df$X.4[i]) > 0){
    x[[length(x)+1]] = 4
  } 
  if(nchar(df$X.5[i]) > 0){
    x[[length(x)+1]] = 5
  } 
  if(nchar(df$X.6[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.7[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.8[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.9[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.10[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.11[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.12[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.13[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.14[i]) > 0){
    x[[length(x)+1]] = 6
  }
  if(nchar(df$X.15[i]) > 0){
    x[[length(x)+1]] = df$X.15[i]
  }
  df$occupation[i] = list(x)
}
 

#  l <- sapply(colnames(df$X.1), function(x) grep("Arts", df$X.1[,x]))
  
  df$language = as.factor(df$language)
  df$Respondent.ID = as.factor(df$Respondent.ID )
  df$Collector.ID = as.factor(df$Collector.ID)
  df$age = as.numeric(as.character(unlist(df$age)))
  
  df$older.or.18 = as.factor(df$older.or.18)
  df$consent = as.factor(df$consent)
  df$gender = as.factor(df$gender)
  df$nationality = as.factor(df$nationality)
  df$nationality = as.factor(df$nationality)
  df$relationship.status = as.factor(df$relationship.status)
  df$cohabitants.underage = as.numeric(df$cohabitants.underage)
    ### RANK df$illness.prone
 
  
  names(df)[names(df) == "Have.you.felt.constantly.under.strain."] <- "CM_1"
  names(df)[names(df) == "Are.were.you.in.quarantine.for..suspected..COVID.19.Â.If.so..where."] <- "quarantine"
  names(df)[names(df) == "Are.were.you.in.quarantine.for..suspected..COVID.19.Â.If.so..where."] <- "quarantine"
  
  
  
  
  
  
  out = df
}
  