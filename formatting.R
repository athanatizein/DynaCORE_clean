#function to adapt variable classes in DynaCORE-C

formatting <- function(df){
  
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
  
  
  #GHQ-12: 
  GHQfactor <- function(x){factor(x, levels = c("Not at all", "No more than usual", "Rather more than usual", "Much more than usual"))}
  term <- "CM"
  variables <- grep(term, names(data_ger))
  data_ger[variables] <- lapply(data_ger[variables], GHQfactor)
  data_ger[variables] <- lapply(data_ger[variables], as.numeric)
  
  #SOZU K-10:
  SOZUfactor <- function(x){factor(x, levels= c("Does not apply at all", "Hardly applies", "Somewhat applies", "Rather applies", "Completely applies" ))}
  term <- "H2_"
  variables <- grep(term, names(data_ger))
  variables <- variables[1:7]
  data_ger[variables] <- lapply(data_ger[variables], SOZUfactor)
  data_ger[variables] <- lapply(data_ger[variables], as.numeric)
  
  '''
  (1) Does not apply at all
  (2) Hardly applies
  (3) Somewhat applies
  (4) Rather applies
  (5) Completely applies 
  '''
  
  }
  