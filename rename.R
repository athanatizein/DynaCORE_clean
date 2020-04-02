rename <- function(df){
  
  names(df)[1] <- "Respondent.ID"
  names(df)[2] <- "Collector.ID"
  names(df)[3] <- "Start.Date"
  names(df)[4] <- "End.Date"
  names(df)[5] <- "Respondent.ID"
  names(df)[6] <- "Respondent.ID"
  names(df)[7] <- "Respondent.ID"
  names(df)[8] <- "Respondent.ID"
  names(df)[9] <- "Respondent.ID"
  names(df)[10] <- "Respondent.ID"
  names(df)[11] <- "Respondent.ID"
  names(df)[1] <- "Respondent.ID"
  names(df)[1] <- "Respondent.ID"
  
  df$language = as.factor(df$language)
  
  df$Respondent.ID = as.factor(df$Respondent.ID )
  df$Collector.ID = as.factor(df$Collector.ID)
  
  names(df)[names(df) == "What.is.your.age."] <- "age"
  df$age = as.numeric(as.character(unlist(df$age)))
  
  names(df)[names(df) == "I.am.18.years.or.older."] <- "older.or.18"
  df$older.or.18 = as.factor(df$older.or.18)
  
  names(df)[names(df) == "I.agreeÂ.that.my.anonymous.answers.will.be.usedÂ.for.a.scientific.study."] <- "consent"
  df$consent = as.factor(df$consent)
  
  names(df)[names(df) == "What.is.the.gender.you.most.identify.with."] <- "gender"
  df$gender = as.factor(df$gender)
  
  names(df)[names(df) == "What.is.your.nationality."] <- "nationality"
  df$nationality = as.factor(df$nationality)
  
  names(df)[names(df) == "In.what.country.do.you.live."] <- "country.residence"
  df$nationality = as.factor(df$nationality)
  
  
  names(df)[names(df) == "In.what.city.town.do.you.live."] <- "city.residence"
  names(df)[names(df) == "Are.you.currently.out.of.town.for.a.longer.period."] <- "away.currently"
  names(df)[names(df) == "If.yes.to..8...in.what.country.are.you.currently.located."] <- "away.country"
  names(df)[names(df) == "If.yes.to..8...in.what.city.are.you.currently.located."] <- "away.city"
  names(df)[names(df) == "How.many.yearsÂ.of.education.have.you.completed.in.total..please.add.elementary.school..high.school..professional.higher.education..university..etc..."] <- "education"
  names(df)[names(df) == "Which.of.the.following.best.describes.your.current.occupation.s...more.than.one.answer.possible.."] <- "occupation(s)"
  names(df)[names(df) == "What.is.your.current.occupationalÂ.status..more.than.one.answer.possible.."] <- "occupational.status"
  names(df)[names(df) == "What.is.your.approximate.average.annual.household.income..please.estimate.in.Euro.."] <- "income"
  names(df)[names(df) == "Which.of.the.following.best.describes.your.current.relationship.status."] <- "relationship.status"
  df$relationship.status = as.factor(df$relationship.status)
  
  names(df)[names(df) == "How.many.people.including.yourself.currently.live.in.your.household."] <- "cohabitants"
  
  names(df)[names(df) == "How.many.of.them.are.under.the.age.of.18."] <- "underage.cohabitants"
  df$underage.cohabitants = as.numeric(df$underage.cohabitants)
  
  names(df)[names(df) == "Compared.to.other.people.of.the.same.age..I.am."] <- "illness.prone"
  ### RANK df$illness.prone
  
  names(df)[names(df) == "Has.a.doctor.or.other.healthcare.provider.EVER.told.you.that.you.have.a.mental.health.condition."] <- "diagnosed.mental.health"
  
  names(df)[names(df) == "Do.you.belong.to.aÂ.risk.group.for.severe.or.life.threatening.symptoms.in.association.with.COVID.19."] <- "risk.group"
  
  names(df)[names(df) == "Have.you.tested.positive.for.Corona.infection."] <- "tested.pos"
  
  names(df)[names(df) == "In.case.you.were.tested.positive..how.severe.were.are.your.COVID.19.symptoms."] <- "tested.pos.symptoms"
  
  names(df)[names(df) == "Are.were.you.in.quarantine.for..suspected..COVID.19.Â.If.so..where."] <- "quarantine"
  
  names(df)[names(df) == "The.set.of.measures.taken.by.the.authorities.to.curtail.the.spread.of.the.Corona.virus.are.both.sensible.and.reasonable."] <- "C22_measure"
  names(df)[names(df) == "I.am.able.to.solve.most.problems.on.my.own."] <- "C23_compliance"
  names(df)[names(df) == "Are.were.you.in.quarantine.for..suspected..COVID.19.Â.If.so..where."] <- "quarantine"
  names(df)[names(df) == "Are.were.you.in.quarantine.for..suspected..COVID.19.Â.If.so..where."] <- "quarantine"
  
  names(df)[names(df) == "Have.you.lost.much.sleep.over.worry."] <- "CM_01"
  names(df)[names(df) == "Have.you.felt.constantly.under.strain."] <- "CM_02"
  names(df)[names(df) == "Have.you.been.able.to.concentrate.on.whatever.you.are.doing."] <- "CM_03"
  names(df)[names(df) == "Have.you.felt.you.were.playing.a.useful.part.in.things."] <- "CM_04"
  names(df)[names(df) == "Have.you.lost.much.sleep.over.worry."] <- "CM_05"
  names(df)[names(df) == "Have.you.felt.constantly.under.strain."] <- "CM_06"
  names(df)[names(df) == "Have.you.lost.much.sleep.over.worry."] <- "CM_07"
  names(df)[names(df) == "Have.you.felt.constantly.under.strain."] <- "CM_08"
  names(df)[names(df) == "Have.you.lost.much.sleep.over.worry."] <- "CM_09"
  names(df)[names(df) == "Have.you.felt.constantly.under.strain."] <- "CM_10"
  names(df)[names(df) == "Have.you.lost.much.sleep.over.worry."] <- "CM_11"
  names(df)[names(df) == "Have.you.felt.constantly.under.strain."] <- "CM_12"
  names(df)[names(df) == "Have.you.lost.much.sleep.over.worry."] <- "CM_13"
  
  names(df)[names(df) == "Have.you.felt.constantly.under.strain."] <- "CM_1"
  names(df)[names(df) == "Are.were.you.in.quarantine.for..suspected..COVID.19.Â.If.so..where."] <- "quarantine"
  names(df)[names(df) == "Are.were.you.in.quarantine.for..suspected..COVID.19.Â.If.so..where."] <- "quarantine"
  
  
  
  
  
  
  out = df
}
  