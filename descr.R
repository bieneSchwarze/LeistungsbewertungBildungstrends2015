#########################################################################
###
### Datendeskription der Variablen, die in den Analysen zur Erklärbarkeit 
### der Halbjahresnote im Fach Deutsch Klasse 9 im Jahr 2015, genutzt 
### werden; Datenquelle IQB Bildungstrends 2015
###
### 26.11.2019
### Sabine Zinn
###
########################################################################
########################################################################

library( Hmisc)
library(readstata13)

# führe prepareData.R aus -> erhalte SE_ (Schülerdaten, die imputiert werden) 

# -------------------------------------------------------------------------------------------------------
# Funktionen zur Deskription: Mittelwert & SD bzw. Anteile und fehlende Werte (gewichtet und ungewichtet)
# -------------------------------------------------------------------------------------------------------
# Variable v ist stetig (metrisch)
getDes1 <- function(v,w){ 
  m <-  mean(v, na.rm = TRUE)
  sd <- sqrt(var(v, na.rm=TRUE))
  mw <- wtd.mean(v,w, na.rm = TRUE)
  sw <- sqrt(wtd.var(v,w, na.rm=TRUE))
  miss <- sum(is.na(v))/length(w)*100
  missw <- sum(w[is.na(v)])/sum(w)*100
  return(round(c(m,sd,miss,mw,sw,missw),2))
}
# Variable v ist binär
getDes2 <- function(v,w){
  m <-  mean(v, na.rm = TRUE)
  mw <- wtd.mean(v,w, na.rm = TRUE)
  miss <- sum(is.na(v))/length(w)*100
  missw <- sum(w[is.na(v)])/sum(w)*100
  return(round(c(m,miss,mw,missw),2))  
}

# ---------------
# Schülerebene
# ---------------
#  Orthografiekompetenz
getDes1(SE_$wle_ortho, SE_$totwgt_deu)
# BEFKI (kognitive Grundfähigkeit)
getDes1(SE_$BEFKIwle, SE_$totwgt_deu)
# 4 Items zur Anstrenungsbereitschaft: intell. Neugierde
getDes1(SE_$Sintne_b, SE_$totwgt_deu)
getDes1(SE_$Sintne_f, SE_$totwgt_deu)
getDes1(SE_$Sintne_i, SE_$totwgt_deu)
getDes1(SE_$Sintne_k, SE_$totwgt_deu)
# Geschlecht des Schülers
getDes2(as.numeric(SE_$TR_geschlecht), SE_$totwgt_deu)
# Schüler hat diagn. SPF (sonderpädag. Förderbedarf)
getDes2(as.numeric(SE_$TR_SPF_r), SE_$totwgt_deu)
# Mutter hat Universitätsabschluss
mtertU <- ifelse(is.na(SE_$Bilm), NA, ifelse(SE_$Bilm %in% 5,1,0))
getDes2(mtertU, SE_$totwgt_deu)
# Mutter hat Bachelor- bzw. Fachhochschulabschluss
mtertFH <- ifelse(is.na(SE_$Bilm), NA, ifelse(SE_$Bilm %in% 4,1,0))
getDes2(mtertFH, SE_$totwgt_deu)
# Mutter hat keinen ter. Bildungsabschluss
mtertNo <- ifelse(is.na(SE_$Bilm), NA, ifelse(SE_$Bilm %in% c(4,5),0,1))
getDes2(mtertNo, SE_$totwgt_deu)
# Schüler hat Migrationshintergrund (ja/nein)
mig <- ifelse(SE_$zhg == 0, 0, 1)
getDes2(mig, SE_$totwgt_deu)
# Interaktion: Schüler ist weiblich und hat Mutter mit Uniabschluss
uniMutterMaed <- ifelse(is.na(SE_$Bilm), NA, ifelse(SE_$Bilm %in% 5 & as.numeric(SE_$TR_geschlecht) %in% 1,1,0))
getDes2(uniMutterMaed, SE_$totwgt_deu)
# Interaktion: Schüler ist weiblich und hat Mutter mit Bachelor-/Fachhochschulabschluss
uniMutterMaed <- ifelse(is.na(SE_$Bilm), NA, ifelse(SE_$Bilm %in% 4 & as.numeric(SE_$TR_geschlecht) %in% 1,1,0))
getDes2(uniMutterMaed, SE_$totwgt_deu)

# ---------------
# Schulebene
# ---------------
SCH <- SE_[order(SE_$IDSCH_FDZ),]
SCH <- SCH[!duplicated(SCH$IDSCH_FDZ),]
# Anzahl Schüler mit SPF an Schule
getDes1(SCH$Pspfges, SCH$totwgt_deu)
# Anzahl Schüler in Klasse
getDes1(SCH$SuS_KL, SCH$totwgt_deu)
# Anteil Mädchen in Klasse
getDes2(SCH$AntMaed_KL, SCH$totwgt_deu)
# Schulart in GY
GY <- ifelse(SCH$RS ==0 & SCH$HS==0 & SCH$MB==0 & SCH$GS==0,1,0)
getDes2(GY, SCH$totwgt_deu)
# Bundeland hat Bildungssystem "modernisierte Strukturen"
getDes2(SCH$SysMod, SCH$totwgt_deu)
# Anteil SuS in Klasse mit Migrationshintergrund
AA <- aggregate(as.numeric(SE_$zhg), list(SCH=SE_$IDSCH_FDZ), mean, na.rm=TRUE) 
table(round(AA$x,1))
getDes1(AA$x, SCH$totwgt_deu)

# ---------------
# Lehrkraftebene
# ---------------
# nimm die imputierten Datensätzen (Imputation nur für Schülerinfos, fehlende Lehrermerkmale werden nicht imputiert)
# jeder imputierte Datensatz enthält ein Lehrkraftgewicht (das unterscheidet sich nun aber von Datensatz zu Datensatz)
setwd("E:\\Projekt_LehrerNoten\\Results")

# Erfahrung als Lehrkraft in Jahren
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$Llja_FDZ, LK$wgt_L))
}
apply(coll,1,mean) # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte

# Geschlecht der Lehrkraft
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$Lgender, LK$wgt_L))
}
apply(coll,1,mean) # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte

# Fortbildung ja
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$FB_diag1, LK$wgt_L))
}
apply(coll,1,mean) # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte

# Fortbildung nein
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$FB_diag0, LK$wgt_L))
}
apply(coll,1,mean) # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte

# Fortbildung unbekannt
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$FB_diag2, LK$wgt_L))
}
apply(coll,1,mean) # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte

# Quereinstieg ja
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$querFremdYes, LK$wgt_L))
}
apply(coll,1,mean)  # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte

# Quereinstieg nein
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$querFremdNo, LK$wgt_L))
}
apply(coll,1,mean)  # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte

# Quereinstieg unbekannt
coll <- NULL
for(iM in 1:20){
  nA <- paste(paste("impData",iM,sep = ""), ".dta", sep="")
  iD <- read.dta13(nA)
  LK <- iD[order(iD$idteach_d_FDZ1),]
  LK <- LK[!duplicated(LK$idteach_d_FDZ1),]
  coll <- cbind(coll, getDes1(LK$querFremdNA, LK$wgt_L))
}
apply(coll,1,mean)  # Werte sind wenig variable zwischen imput. Datensätzen, daher nimm Druchschnittswerte



