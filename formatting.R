#function to adapt variable classes in DynaCORE-C

formatting <- function(data){
  
  df = data
################### confounds ########################

  # While it is possible to use grepl to match the strings in multiple choice questions, I was not sure if the text will be different depending on the language of the survey. So I tried a workaround based on position in the survey only - LP
  
  # to do based on text, use:
  #  l <- sapply(colnames(df$X.1), function(x) grep("Arts", df$X.1[,x]))
  

### collect all occupations in a list #####
  
  for(i in 1:length(df$Respondent.ID)){
    x <- vector()
    if(!is.na(df$occupation[i])){
        if(nchar(df$occupation[i]) > 0){
        x[length(x)+1] = 1
        }
      }
     if(!is.na(df$X[i]) && nchar(df$X[i]) > 0){
      x[length(x)+1] = 2
    }
    if(!is.na(df$X.1[i]) && nchar(df$X.1[i]) > 0){
      x[length(x)+1] = 3
    }
    if(!is.na(df$X.1[i]) && nchar(df$X.2[i]) > 0){
      x[length(x)+1] = 4
    }
    if(!is.na(df$X.3[i]) && nchar(df$X.3[i]) > 0){
      x[length(x)+1] = 5
    }
    if(!is.na(df$X.4[i]) && nchar(df$X.4[i]) > 0){
      x[length(x)+1] = 6
    }
    if(!is.na(df$X.5[i]) && nchar(df$X.5[i]) > 0){
      x[length(x)+1] = 7
    }
    if(!is.na(df$X.6[i]) && nchar(df$X.6[i]) > 0){
      x[length(x)+1] = 8
    }
    if(!is.na(df$X.7[i]) && nchar(df$X.7[i]) > 0){
      x[length(x)+1] = 9
    }
    if(!is.na(df$X.8[i]) && nchar(df$X.8[i]) > 0){
      x[length(x)+1] = 10
    }
    if(!is.na(df$X.9[i]) && nchar(df$X.9[i]) > 0){
      x[length(x)+1] = 11
    }
    if(!is.na(df$X.10[i]) && nchar(df$X.10[i]) > 0){
      x[length(x)+1] = 12
    }
    if(!is.na(df$X.11[i]) && nchar(df$X.11[i]) > 0){
      x[length(x)+1] = 13
    }
    if(!is.na(df$X.12[i]) && nchar(df$X.12[i]) > 0){
      x[length(x)+1] = 14
    }
    if(!is.na(df$X.13[i]) && nchar(df$X.13[i]) > 0){
      x[length(x)+1] = 15
    }
    if(!is.na(df$X.14[i]) && nchar(df$X.14[i]) > 0){
      x[length(x)+1] = 16
    }
    if(!is.na(df$X.15[i]) && nchar(df$X.15[i]) > 0){
      x[length(x)+1] = df$X.15[i]
    }
    df$occupation[i] = list(x)
  }
  
### collect occupational status in a list #####
  
  for(i in 1:length(df$Respondent.ID)){
    x <- vector()
    if(!is.na(df$occupational.status[i]) && nchar(df$occupational.status[i]) > 0){
      x[length(x)+1] = 1
    }
    if(!is.na(df$X.16[i]) && nchar(df$X.16[i]) > 0){
      x[length(x)+1] = 2
    }
    if(!is.na(df$X.17[i]) && nchar(df$X.17[i]) > 0){
      x[length(x)+1] = 3
    }
    if(!is.na(df$X.18[i]) && nchar(df$X.18[i]) > 0){
      x[length(x)+1] = 4
    }
    if(!is.na(df$X.19[i]) && nchar(df$X.19[i]) > 0){
      x[length(x)+1] = 5
    }
    if(!is.na(df$X.20[i]) && nchar(df$X.20[i]) > 0){
      x[length(x)+1] = 6
    }
    if(!is.na(df$X.21[i]) && nchar(df$X.21[i]) > 0){
      x[length(x)+1] = 7
    }
    if(!is.na(df$X.22[i]) && nchar(df$X.22[i]) > 0){
      x[length(x)+1] = 8
    }
    if(!is.na(df$X.23[i]) && nchar(df$X.23[i]) > 0){
      x[length(x)+1] = 9
    }
    if(!is.na(df$X.24[i]) && nchar(df$X.24[i]) > 0){
      x[length(x)+1] = 10
    }
    if(!is.na(df$X.25[i]) && nchar(df$X.25[i]) > 0){
      x[length(x)+1] = 11
    }
    if(!is.na(df$X.26[i]) && nchar(df$X.26[i]) > 0){
      x[length(x)+1] = 12
    }
    if(!is.na(df$X.27[i]) && nchar(df$X.27[i]) > 0){
      x[length(x)+1] = df$X.27[i]
    }
    df$occupational.status[i] = list(x)
  }
  
  
  #   
  # #GHQ-12: 
  # GHQfactor <- function(x){factor(x, levels = c("Not at all", "No more than usual", "Rather more than usual", "Much more than usual"))}
  # term <- "CM"
  # variables <- grep(term, names(data_ger))
  # data_ger[variables] <- lapply(data_ger[variables], GHQfactor)
  # data_ger[variables] <- lapply(data_ger[variables], as.numeric)
  # 
  # #SOZU K-10:
  # SOZUfactor <- function(x){factor(x, levels= c("Does not apply at all", "Hardly applies", "Somewhat applies", "Rather applies", "Completely applies" ))}
  # term <- "H2_"
  # variables <- grep(term, names(data_ger))
  # variables <- variables[1:7]
  # data_ger[variables] <- lapply(data_ger[variables], SOZUfactor)
  # data_ger[variables] <- lapply(data_ger[variables], as.numeric)
  # 
  # '''
  # (1) Does not apply at all
  # (2) Hardly applies
  # (3) Somewhat applies
  # (4) Rather applies
  # (5) Completely applies 
  # '''
  # 
  
  return(df)
  
  }
  