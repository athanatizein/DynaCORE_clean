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
  names(df)[81] <- "H2_01"
  names(df)[82] <- "H2_02"
  names(df)[83] <- "H2_03"
  names(df)[84] <- "H2_04"
  names(df)[85] <- "H2_05"
  names(df)[86] <- "H2_06"
  names(df)[87] <- "H2_07"
  names(df)[88] <- "H2_08"
  names(df)[89] <- "H3_01"
  names(df)[90] <- "H4_01"
  names(df)[91] <- "H4_02"
  names(df)[92] <- "H4_03"
  names(df)[93] <- "H5_01"
  names(df)[94] <- "H5_02"
  names(df)[95] <- "H5_03"
  names(df)[96] <- "H5_04"
  names(df)[97] <- "H5_05"
  names(df)[98] <- "H5_06"
  names(df)[99] <- "H6_01"
  names(df)[100] <- "H6_02"
  names(df)[101] <- "H1_COPE_05"
  names(df)[102] <- "H1_COPE_09"
  names(df)[103] <- "H1_COPE_10"
  names(df)[104] <- "H1_COPE_14"
  names(df)[105] <- "H1_COPE_15"
  names(df)[106] <- "H1_COPE_18"
  names(df)[107] <- "H1_COPE_21"
  names(df)[108] <- "H1_COPE_23"
  names(df)[109] <- "H1_COPE_25"
  names(df)[110] <- "H1_COPE_28"
  names(df)[111] <- "H1_CERQ_02"
  names(df)[112] <- "H1_CERQ_04"
  names(df)[113] <- "H1_CERQ_06"
  names(df)[114] <- "H1_CERQ_11"
  names(df)[115] <- "H1_CERQ_14"
  names(df)[116] <- "H1_CERQ_15"
  names(df)[117] <- "H1_CERQ_16"
  names(df)[118] <- "H1_CERQ_22"
  names(df)[119] <- "H1_CERQ_23"
  names(df)[120] <- "H1_CERQ_25"
  names(df)[121] <- "H1_CERQ_28"
  names(df)[122] <- "H1_CERQ_29"
  names(df)[123] <- "H1_Cor_01"
  names(df)[124] <- "H1_Cor_02"
  names(df)[125] <- "CE_01"
  names(df)[126] <- "CE_02"
  names(df)[127] <- "CE_03"
  names(df)[128] <- "CE_04"
  names(df)[129] <- "CE_05"
  names(df)[130] <- "CE_06"
  names(df)[131] <- "CE_07"
  names(df)[132] <- "CE_08" 
  names(df)[133] <- "CE_09"
  names(df)[134] <- "CE_10"
  names(df)[135] <- "CE_11"
  names(df)[136] <- "CE_12"
  names(df)[137] <- "CE_13"
  names(df)[138] <- "CE_14"
  names(df)[139] <- "CE_15"
  names(df)[140] <- "CE_16"
  names(df)[141] <- "CE_17"
  names(df)[142] <- "CE_18"
  names(df)[143] <- "CE_19"
  names(df)[144] <- "CE_20"
  names(df)[145] <- "CE_21"
  names(df)[146] <- "CE_22"
  names(df)[147] <- "CE_23"
  names(df)[148] <- "CE_24"
  names(df)[149] <- "CE_25"
  names(df)[150] <- "CE_26"
  names(df)[151] <- "CE_27"
  names(df)[152] <- "CE_28"
  names(df)[153] <- "CE_29"
  names(df)[154] <- "CE_30"
  names(df)[155] <- "CE_30_text"
  names(df)[156] <- "GE_01"
  names(df)[157] <- "GE_02"
  names(df)[158] <- "GE_03"
  names(df)[159] <- "GE_04"
  names(df)[160] <- "GE_05"
  names(df)[161] <- "GE_06"
  names(df)[162] <- "GE_07"
  names(df)[163] <- "GE_08"
  names(df)[164] <- "GE_09"
  names(df)[165] <- "GE_10"
  names(df)[166] <- "GE_11"
  names(df)[167] <- "GE_12"
  names(df)[168] <- "GE_12_text"
  names(df)[169] <- "O_01"
  names(df)[170] <- "O_02"
  names(df)[171] <- "O_03"
  
  
  
# While it is possible to use grepl to match the strings in multiple choice questions, I was not sure if the text will be different depending on the language of the survey. So I tried a workaround based on position in the survey only - LP
  
#x <- vector(mode = "list", length = 0)
  for(i in 1:length(df$Respondent.ID)){
    #x <- vector(mode = "any", length = 0)
    x <- vector()
    if(nchar(df$X.1[i]) > 0){
      x[length(x)+1] = 1
    } 
    if(nchar(df$X.2[i]) > 0){
      x[length(x)+1] = 2
    } 
    if(nchar(df$X.3[i]) > 0){
      x[length(x)+1] = 3
    } 
    if(nchar(df$X.4[i]) > 0){
      x[length(x)+1] = 4
    } 
    if(nchar(df$X.5[i]) > 0){
      x[length(x)+1] = 5
    } 
    if(nchar(df$X.6[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.7[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.8[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.9[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.10[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.11[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.12[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.13[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.14[i]) > 0){
      x[length(x)+1] = 6
    }
    if(nchar(df$X.15[i]) > 0){
      x[length(x)+1] = df$X.15[i]
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
  