#function to adapt variable classes in DynaCORE-C

formatting <- function(data){
  
  df = data
################### confounds ########################
  
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
  
  # While it is possible to use grepl to match the strings in multiple choice questions, I was not sure if the text will be different depending on the language of the survey. So I tried a workaround based on position in the survey only - LP
  
  # to do based on text, use:
  #  l <- sapply(colnames(df$X.1), function(x) grep("Arts", df$X.1[,x]))
  

### collect all occupations in a list #####
  
  for(i in 1:length(df$Respondent.ID)){
    x <- vector()
    if(nchar(df$occupation[i]) > 0){
      x[length(x)+1] = 1
    }
     if(nchar(df$X[i]) > 0){
      x[length(x)+1] = 2
    }
    if(nchar(df$X.1[i]) > 0){
      x[length(x)+1] = 3
    }
    if(nchar(df$X.2[i]) > 0){
      x[length(x)+1] = 4
    }
    if(nchar(df$X.3[i]) > 0){
      x[length(x)+1] = 5
    }
    if(nchar(df$X.4[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.5[i]) > 0){
      x[length(x)+1] = 7
    }
    if(nchar(df$X.6[i]) > 0){
      x[length(x)+1] = 8
    }
    if(nchar(df$X.7[i]) > 0){
      x[length(x)+1] = 9
    }
    if(nchar(df$X.8[i]) > 0){
      x[length(x)+1] = 10
    }
    if(nchar(df$X.9[i]) > 0){
      x[length(x)+1] = 11
    }
    if(nchar(df$X.10[i]) > 0){
      x[length(x)+1] = 12
    }
    if(nchar(df$X.11[i]) > 0){
      x[length(x)+1] = 13
    }
    if(nchar(df$X.12[i]) > 0){
      x[length(x)+1] = 14
    }
    if(nchar(df$X.13[i]) > 0){
      x[length(x)+1] = 15
    }
    if(nchar(df$X.14[i]) > 0){
      x[length(x)+1] = 16
    }
    if(nchar(df$X.15[i]) > 0){
      x[length(x)+1] = df$X.15[i]
    }
    df$occupation[i] = list(x)
  }
  
### collect occupational status in a list #####
  
  for(i in 1:length(df$Respondent.ID)){
    x <- vector()
    if(nchar(df$occupational.status[i]) > 0){
      x[length(x)+1] = 1
    }
    if(nchar(df$X.16[i]) > 0){
      x[length(x)+1] = 2
    }
    if(nchar(df$X.17[i]) > 0){
      x[length(x)+1] = 3
    }
    if(nchar(df$X.18[i]) > 0){
      x[length(x)+1] = 4
    }
    if(nchar(df$X.19[i]) > 0){
      x[length(x)+1] = 5
    }
    if(nchar(df$X.20[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.21[i]) > 0){
      x[length(x)+1] = 7
    }
    if(nchar(df$X.22[i]) > 0){
      x[length(x)+1] = 8
    }
    if(nchar(df$X.23[i]) > 0){
      x[length(x)+1] = 9
    }
    if(nchar(df$X.24[i]) > 0){
      x[length(x)+1] = 10
    }
    if(nchar(df$X.25[i]) > 0){
      x[length(x)+1] = 11
    }
    if(nchar(df$X.26[i]) > 0){
      x[length(x)+1] = 12
    }
    if(nchar(df$X.27[i]) > 0){
      x[length(x)+1] = df$X.27[i]
    }
    df$occupational.status[i] = list(x)
  }

    # extract time from date variable
    # as.Date(data_en$)
    
    # rank df$illness.prone
  
  data_en[,c(68:154,156:167)] <- lapply(data_en[,c(68:154,156:167)], as.numeric)
  
  #GHQ: 
  term <- "CM"
  variables <- grep(term, names(data_en))
  GHQrecode <- function(x){recode(x, '1'=0L, '2'=1L, '3'=2L, '4'=3L)}
  data_en[variables] <- lapply(data_en[variables], GHQrecode)
  
  #SOZU: doesnt need recoding
  
  
  
  
  '''  
  #GHQ-12: 
  GHQfactor <- function(x){factor(x, levels = c("Not at all", "No more than usual", "Rather more than usual", "Much more than usual"))}
  term <- "CM"
  variables <- grep(term, names(data_en))
  #data_ger[variables] <- lapply(data_ger[variables], GHQfactor)
  #data_ger[variables] <- lapply(data_ger[variables], as.numeric)
  
  #SOZU K-10:
  SOZUfactor <- function(x){factor(x, levels= c("Does not apply at all", "Hardly applies", "Somewhat applies", "Rather applies", "Completely applies" ))}
  term <- "H2_"
  variables <- grep(term, names(data_ger))
  variables <- variables[1:7]
  data_ger[variables] <- lapply(data_ger[variables], SOZUfactor)
  data_ger[variables] <- lapply(data_ger[variables], as.numeric)
  
  
  (1) Does not apply at all
  (2) Hardly applies
  (3) Somewhat applies
  (4) Rather applies
  (5) Completely applies 
  '''
  
  
  return(df)
  
  }
  