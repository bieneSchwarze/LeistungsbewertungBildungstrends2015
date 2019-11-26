####################################################################
###
### Datenaufbereitung der IQB Bildungstrends 2015
### (R Syntax, Version 3.5.1)
### 16.11.2019
### Sabine Zinn
###
####################################################################
####################################################################

rm(list=ls())

library(readstata13)
library(mice)
library(psych)
library(Hmisc)
library(polycor)
library(miceadds)
library(micemd)
library(survey)
library(wCorr)
library(questionr)
library(foreign)
library(psych)
setwd("F:\\IQB_Paper\\Projekt_LehrerNoten\\IQB-Daten\\1809-12a\\1809-12a\\Stata\\IQB-BT 2015")

S0 <- read.dta13("IQB-BT-2015_Schuelerfragebogen_SUF_1809-12a.dta")
L0 <- read.dta13("IQB-BT-2015_DE_Eng_Lehrerfragebogen_SUF_v4.dta")
H0 <- read.dta13("IQB-BT-2015_Schulleiterfragebogen_SUF_1809-12a.dta")

nam <- c("IDSTUD_FDZ", "IDSCH_FDZ", "idteach_d_FDZ1", "schulart", "System_Klass4_FDZ", "STUDY_TYPE", "TR_LEFT", 
         "TR_EXCLUSION", "TR_geschlecht", "TR_MONTH", "TR_YEAR_FDZ", "Sabildm", "Eabildm", "Sabildv", "Eabildv",
         "zhg", "Suebsp_h", "Swied_i", "TR_SPF_r", "BEFKIwle", "trnotedeu_r", "wle_lesen", "wle_ortho", "wle_hoeren", 
         "wle_reading", "wle_listening", "totwgt_deu", "Sintne_b", "Sintne_f", "Sintne_i", "Sintne_k")
nam[!(nam %in% names(S0))]
S <- S0[, nam]
S <- S[S$totwgt_deu>0,] # nimm diejenigen aus der Stichprobe, die keine gemessenen Deutschkompetenzen haben & somit kein Fallgewicht (N=244 NA & N=3462 zeros)
table(S[S$totwgt_deu<=0,"totwgt_deu"])
S <- S[!is.na(S$totwgt_deu),]
table(is.na(S$totwgt_deu))
dim(S)

table(S$schulart, exclude = NULL) 
S$schulart <- as.character(S$schulart)
S$schulart[S$schulart=="Hauptschule"] <- "HS"
S$schulart[S$schulart=="Realschule"] <- "RS"
S$schulart[S$schulart=="Gesamtschule"] <- "GS"
S$schulart[S$schulart=="Schule mit mehreren Bildungsgängen
"] <- "MB"
S$schulart[S$schulart=="Gymnasium"] <- "GY"
S$schulart[S$schulart=="Förderschule"] <- "FS"

table(S$System_Klass4_FDZ, exclude = NULL)
S$System_Klass4_FDZ <- as.character(S$System_Klass4_FDZ)
S$System_Klass4_FDZ[S$System_Klass4_FDZ=="Modernisierte Strukturen (BB HB HE HH MV BE NW RP SH SL)"] <- "MOD"
S$System_Klass4_FDZ[S$System_Klass4_FDZ=="Mischtyp, traditionell (BW BY SN)"] <- "MTTR"
S$System_Klass4_FDZ[S$System_Klass4_FDZ=="Mischtyp, modernisiert (NI ST TH)"] <- "MTMOD"

table(S$STUDY_TYPE, exclude = NULL) 
S$STUDY_TYPE <- as.character(S$STUDY_TYPE)
S$STUDY_TYPE[S$STUDY_TYPE == "Deutsch/Englisch"] <- "DE"
S$STUDY_TYPE[S$STUDY_TYPE == "Foerderschulen"] <- "FOE"

table(S$TR_LEFT, exclude=NULL) # okay
table(S$TR_EXCLUSION, exclude=NULL) # okay, entferne beide Variablen: niemand hat den Test verlassen oder wurde ausgeschlossen
S <- S[,-which(colnames(S) %in% c("TR_LEFT", "TR_EXCLUSION"))]

mm <- as.numeric(S$TR_MONTH) - 1
my <- as.character(S$TR_YEAR_FDZ)
my[my == "1995-1996"] <- 1995
my[my == "2001-2002"] <- 2001
table(my, exclude=NULL) # N=245 SuS unbekannt
my <- as.numeric(my)
S$CMC <- 12*(my-1900)+mm
S <- S[,-which(colnames(S) %in% c("TR_MONTH", "TR_YEAR_FDZ"))]

S$Sabildm <- as.character(S$Sabildm)
S$Eabildm <- as.character(S$Eabildm)
table(S$Eabildm, exclude=NULL)
S$Eabildm[is.na(S$Eabildm)] <- S$Sabildm[is.na(S$Eabildm)]
S$Eabildm <- as.character(S$Eabildm)
table(S$Eabildm, exclude=NULL)
unique(S$Eabildm)
S$Eabildm[S$Eabildm == "Abschluss an einer Fach-, Meister- oder Technikerschule, /einer Berufsakademie oder Fachakademie"] <- "FS"
S$Eabildm[S$Eabildm == "Abschluss an einer Fach-, Meister- oder Technikerschule, Berufsakademie oder Fachakademie"] <- "FS"
S$Eabildm[S$Eabildm == "Bachelor (an Hochschule oder Fachhochschule)"] <- "FHBA"
S$Eabildm[S$Eabildm == "Fachhochschulabschluss  (z. B. Diplom (FH))"] <- "FHBA"
S$Eabildm[S$Eabildm == "Fachhochschulabschluss (z. B. Diplom (FH))"] <- "FHBA"
S$Eabildm[S$Eabildm == "Beruflich-betriebliche Ausbildung (Lehre)"] <- "AUS"
S$Eabildm[S$Eabildm == "Beruflich-schulische Ausbildung (Berufsfach-/Handelsschule, mittl. Dienst i. d. oeff. Verwaltung) (Wortlaut gekuerzt)"] <- "AUS"
S$Eabildm[S$Eabildm == "Sonstiger beruflicher Abschluss (z. B. im Ausland)"] <- "NIX"
S$Eabildm[S$Eabildm == "Promotion (Doktorpruefung)"] <- "UNI"
S$Eabildm[S$Eabildm == "Universitaetsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)" ] <- "UNI"
S$Eabildm[S$Eabildm == "Kein beruflicher Ausbildungsabschluss und nicht mehr in Ausbildung"] <- "NIX"
S$Eabildm[S$Eabildm == "Noch in beruflicher Ausbildung (Berufsvorbereitungsjahr, Auszubildende, Praktikantin, Studentin) (Wortlaut gekuerzt)"] <- "NIX"
table(S$Eabildm, exclude=NULL)
S$Eabildm[S$Eabildm == names(table(S$Eabildm, exclude=NULL))[2]] <- "FHBA" # irgendwas ist hier komisch, aber so geht es nun doch

S$Sabildv <- as.character(S$Sabildv)
S$Eabildv <- as.character(S$Eabildv)
table(S$Eabildv, exclude=NULL)
S$Eabildv[is.na(S$Eabildv)] <- S$Sabildv[is.na(S$Eabildv)]
S$Eabildv <- as.character(S$Eabildv)
table(S$Eabildv, exclude=NULL)
unique(S$Eabildv)
S$Eabildv[S$Eabildv == "Abschluss an einer Fach-, Meister- oder Technikerschule, /einer Berufsakademie oder Fachakademie"] <- "FS"
S$Eabildv[S$Eabildv == "Abschluss an einer Fach-, Meister- oder Technikerschule, Berufsakademie oder Fachakademie"] <- "FS"
S$Eabildv[S$Eabildv == "Bachelor (an Hochschule oder Fachhochschule)"] <- "FHBA"
S$Eabildv[S$Eabildv == "Fachhochschulabschluss  (z. B. Diplom (FH))"] <- "FHBA"
S$Eabildv[S$Eabildv == "Fachhochschulabschluss (z. B. Diplom (FH))"] <- "FHBA"
S$Eabildv[S$Eabildv == "Beruflich-betriebliche Ausbildung (Lehre)"] <- "AUS"
S$Eabildv[S$Eabildv == "Beruflich-schulische Ausbildung (Berufsfach-/Handelsschule, mittl. Dienst i. d. oeff. Verwaltung) (Wortlaut gekuerzt)"] <- "AUS"
S$Eabildv[S$Eabildv == "Sonstiger beruflicher Abschluss (z. B. im Ausland)"] <- "NIX"
S$Eabildv[S$Eabildv == "Promotion (Doktorpruefung)"] <- "UNI"
S$Eabildv[S$Eabildv == "Universitaetsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)" ] <- "UNI"
S$Eabildv[S$Eabildv == "Kein beruflicher Ausbildungsabschluss und nicht mehr in Ausbildung"] <- "NIX"
S$Eabildv[S$Eabildv == "Noch in beruflicher Ausbildung (Berufsvorbereitungsjahr, Auszubildende, Praktikantin, Studentin) (Wortlaut gekuerzt)"] <- "NIX"
table(S$Eabildv, exclude=NULL)
S$Eabildv[S$Eabildv == names(table(S$Eabildv, exclude=NULL))[2]] <- "FHBA" # irgendwas ist hier komisch, aber so geht es nun doch
S <- S[,-which(colnames(S) %in% c("Sabildm", "Sabildv"))]

table(S$zhg, exclude=NULL)
S$zhg <- as.character(S$zhg)
S$zhg[S$zhg == "Ohne Zuwanderungshintergrund"] <- 0
S$zhg[S$zhg == "Zweite Generation"] <- 1
S$zhg[S$zhg == "Erste Generation"] <- 2
S$zhg[S$zhg == "Ein Elternteil im Ausland geboren"] <- 3 # 2.5 Generation
#S$zhg[is.na(S$zhg)] <- 4

table(S$Suebsp_h, exclude=NULL) # Info schlecht, sehr wenige SuS haben Überspringer angekreuzt (N=97 von 33080): 0,3%
table(S$Swied_i, exclude=NULL) # bei Wiederholern: Info besser
S$Swied_i <- as.character(S$Swied_i)
S$Swied_i[S$Swied_i=="nicht angekreuzt"] <- 0
S$Swied_i[S$Swied_i=="angekreuzt"] <- 1
S$Swied_i[is.na(S$Swied_i)] <- 2 # allerdings mehr NA als Wiederholer, nimm die Var raus
S <- S[,-which(colnames(S) %in% c("Swied_i", "Suebsp_h"))]

table(S$TR_SPF_r, exclude=NULL)
S$TR_SPF_r <- as.character(S$TR_SPF_r)
S$TR_SPF_r[S$TR_SPF_r=="ja, FSP lernen"] <- 1
S$TR_SPF_r[S$TR_SPF_r=="ja, anderer FSP"] <- 1
S$TR_SPF_r[S$TR_SPF_r=="nein"] <- 0
table(S$STUDY_TYPE, S$TR_SPF_r)

table(S$trnotedeu_r, exclude=NULL) # fehlt für 2813 Fälle, entspricht ca. 8,5% des Samples 
S$trnotedeu_r <- as.character(S$trnotedeu_r)
S$trnotedeu_r[S$trnotedeu_r == "sehr gut"] <- 1
S$trnotedeu_r[S$trnotedeu_r == "gut"] <- 2
S$trnotedeu_r[S$trnotedeu_r == "befriedigend"] <- 3
S$trnotedeu_r[S$trnotedeu_r == "ausreichend"] <- 4
S$trnotedeu_r[S$trnotedeu_r == "mangelhaft"] <- 5
S$trnotedeu_r[S$trnotedeu_r == "ungenuegend"] <- 6

S$Sintne_b <- as.character(S$Sintne_b)
table(S$Sintne_b, exclude=NULL)
S$Sintne_b[S$Sintne_b=="Trifft ueberhaupt nicht zu"] <- 1
S$Sintne_b[S$Sintne_b=="Trifft eher nicht zu"] <- 2
S$Sintne_b[S$Sintne_b=="Trifft eher zu"] <- 3
S$Sintne_b[S$Sintne_b=="Trifft etwas zu"] <- 4
S$Sintne_b[S$Sintne_b=="Trifft voll und ganz zu"] <- 5
S$Sintne_f <- as.character(S$Sintne_f)
S$Sintne_f[S$Sintne_f=="Trifft ueberhaupt nicht zu"] <- 1
S$Sintne_f[S$Sintne_f=="Trifft eher nicht zu"] <- 2
S$Sintne_f[S$Sintne_f=="Trifft eher zu"] <- 3
S$Sintne_f[S$Sintne_f=="Trifft etwas zu"] <- 4
S$Sintne_f[S$Sintne_f=="Trifft voll und ganz zu"] <- 5
table(S$Sintne_f, exclude=NULL)
S$Sintne_i <- as.character(S$Sintne_i)
S$Sintne_i[S$Sintne_i=="Trifft ueberhaupt nicht zu"] <- 1
S$Sintne_i[S$Sintne_i=="Trifft eher nicht zu"] <- 2
S$Sintne_i[S$Sintne_i=="Trifft eher zu"] <- 3
S$Sintne_i[S$Sintne_i=="Trifft etwas zu"] <- 4
S$Sintne_i[S$Sintne_i=="Trifft voll und ganz zu"] <- 5
table(S$Sintne_i, exclude=NULL)
S$Sintne_k <- as.character(S$Sintne_k)
S$Sintne_k[S$Sintne_k=="Trifft ueberhaupt nicht zu"] <- 1
S$Sintne_k[S$Sintne_k=="Trifft eher nicht zu"] <- 2
S$Sintne_k[S$Sintne_k=="Trifft eher zu"] <- 3
S$Sintne_k[S$Sintne_k=="Trifft etwas zu"] <- 4
S$Sintne_k[S$Sintne_k=="Trifft voll und ganz zu"] <- 5
table(S$Sintne_k, exclude=NULL)

H <- md.pattern(S, plot=FALSE) 
round(H[nrow(H),]*100/nrow(S),2)

H <- H0[, c("IDSCH_FDZ", "Pstu9jg_FDZ", "Pstudeu", "Pewsort" , "Pgtb", "Pspfges")]
table(H$Pstu9jg_FDZ, exclude = NULL)
H$Pstu9jg_FDZ <- as.numeric(as.character(H$Pstu9jg_FDZ))
table(H$Pstudeu, exclude=NULL)
H$Pstudeu <- as.character(H$Pstudeu)
H$Pstudeu[H$Pstudeu=="Mehr als 90 %"] <- 4
H$Pstudeu[H$Pstudeu=="76-90 %"] <- 3
H$Pstudeu[H$Pstudeu=="51-75 %"] <- 2
H$Pstudeu[H$Pstudeu=="26-50 %"] <- 1
H$Pstudeu[H$Pstudeu=="25 % oder weniger"] <- 0
head(H)
H$Pgtb <- ifelse(H$Pgtb == "Ja", 1,0)
H$Pewsort <- as.character(H$Pewsort)
H$Pewsort[H$Pewsort == "3.000 Einwohnerinnen und Einwohner"] <- 0
H$Pewsort[H$Pewsort == "Bis 15.000 Einwohnerinnen und Einwohner"] <- 1
H$Pewsort[H$Pewsort == "Bis 50.000 Einwohnerinnen und Einwohner"] <- 2
H$Pewsort[H$Pewsort == "Bis 100.000 Einwohnerinnen und Einwohner"] <- 3
H$Pewsort[H$Pewsort == "Bis 500.000 Einwohnerinnen und Einwohner"] <- 4
H$Pewsort[H$Pewsort == "Mehr als 500.000 Einwohnerinnnen und Einwohner"] <- 5
H$Pewsort <- as.numeric(H$Pewsort)
SE <- merge(S, H[-which(H$IDSCH_FDZ==245)[2],], by="IDSCH_FDZ", all.x=TRUE)

SE_ <- SE[SE$STUDY_TYPE %in% "DE",] # N = 31594 von 32836 
length(unique(SE_$idteach_d_FDZ1))

L <- L0[, c("IDSCH_FDZ", "idteach_FDZ", "Lage_FDZ", "Lgender", "Llja_FDZ", "Llbfb_a", "Lquest", "Lfremd_deu", "Lquer_deu")]
table(L$Lage_FDZ, exclude=NULL)
table(L$Lgender, exclude=NULL)
L$Lgender <- as.character(L$Lgender)
table(as.numeric(L$Llja_FDZ), exclude=NULL)
L$Llja_FDZ <- as.numeric(L$Llja_FDZ)
table(L$Llbfb_a, exclude=NULL) # unterrichtet gerade Deutsch (in 2015)
L$Llbfb_a <- as.character(L$Llbfb_a)
table(L$Lquest, exclude=NULL)
table(is.na(SE_$idteach_d_FDZ1)) # N=23321 SuS mit LehrerID, N=8273 ohne LehrerID -> überprüfe, wer diese N=8273 SuS ohne LehrerID sind
MM <- SE_[is.na(SE_$idteach_d_FDZ1),] # alle Studie Deutsch/Englisch, mit Deutsch wle in verschiedenen Domainen
length(unique(MM$IDSCH_FDZ)) # betrifft 396 Schulen von 1425 im red. Sample (ohne FOE) --> betrifft ca. 28% aller Schulen und 26% aller SuS -> diese fliegen raus
FB <- read.dta13("bt2015_lehrer_fort.dta")
FB$fort_diag[is.na(FB$fort_diag)] <- 2
table(FB$idteach_FDZ %in% L$idteach_FDZ) # alle drin
L <- merge(L, FB[, c("idteach_FDZ", "fort_diag")], by="idteach_FDZ", all.x = TRUE)
L$fort_diag[is.na(L$fort_diag)] <- 2
L$querfremd <- ifelse(L$Lfremd_deu == 1 | L$Lquer_deu == 1, 1, ifelse(L$Lfremd_deu == 0 & L$Lquer_deu == 0, 0, 2)) # 56% Missings!
L <- L[,-which(colnames(L) %in% c("Lfremd_deu", "Lquer_deu"))]

SLn <- merge(SE_, L, by.x ="idteach_d_FDZ1", by.y="idteach_FDZ", all.x = TRUE) # N=23321 
SLn <- SLn[is.na(SLn$idteach_d_FDZ1),]  # N=8273 ohne LehrerID
sch0 <- unique(SLn$IDSCH_FDZ.x) # betrifft N=396 Schulen
 
# # überpüfe Fälle, die zur Gruppe gehören, die Überzogen wurde: SuS mit SPF Lernen, Sprache, emot. & soz. Entwicklung -> das sind die SPF Fälle mit einem niedrigem Fallgewicht in der SuS-Gruppe pro Schule
# idL <- unique(SE_$IDSCH_FDZ) # N=1425 Schulen in Sample (ohne FOE)
# SF_ <- NULL
# lle <- c()
# for(id in idL) {
#   mm <- SE_[SE_$IDSCH_FDZ == id,]
#   ww <- unique(mm$totwgt_deu)
#   lle <- c(lle, length(ww))
#   if(length(ww)==2){
#     wm <- min(ww)
#     ii1 <- which(mm$totwgt_deu==wm)
#     ii2 <- which(mm$TR_SPF_r %in% c(1, NA))
#     mm <- mm[- intersect(ii1,ii2),]
#   }
#   SF_ <- rbind(SF_, mm)
# }
# table(lle) # von N=1425 Schulen, in 40 Schulen SuS mit SPF überzogen (zusätzlich SuS aus anderen Klassen in Stichprobe), ansonsten haben alle Schulen nur EIN Gewicht 
# -> 31354 SuS ohne SPF in der interessiernden Gruppe
# -> belasse diese Kinder dennoch in Stichprobe, da ich sie nicht sauber von den anderen Kinder getrennt bekomme (ist natürlich schwierig, bei den Aggregatemaßen, weil hier noch SPF SuS aus anderen Klassen hinzukommen)

# füge Aggregate hinzu: Anteil Mädchen + Anzahl SuS in Klasse (später noch Anteil Mig und Anteil SES)
AA <- aggregate(SE_$IDSTUD_FDZ, list(SCH=SE_$IDSCH_FDZ), length) 
table(AA$x)
AA[AA$x==4,] # GS & RS
colnames(AA)[2] <- "SuS_KL"
SE_ <- merge(SE_, AA, by.x="IDSCH_FDZ", by.y="SCH")
AA <- aggregate(as.numeric(SE_$TR_geschlecht)-2, list(SCH=SE_$IDSCH_FDZ), mean) 
table(round(AA$x,1))
colnames(AA)[2] <- "AntMaed_KL"
SE_ <- merge(SE_, AA, by.x="IDSCH_FDZ", by.y="SCH")

# imputiere Daten
H <- md.pattern(SE_, plot=FALSE)
round(H[nrow(H),]*100/nrow(S),2)

table(SE_$System_Klass4_FDZ)
SE_$SysMod <- ifelse(SE_$System_Klass4_FDZ == "MOD", 1, 0)
SE_$SysMTMOD <- ifelse(SE_$System_Klass4_FDZ == "MTMOD", 1, 0)
SE_$RS <- ifelse(SE_$schulart=="RS",1,0)
SE_$HS <- ifelse(SE_$schulart=="HS",1,0)
SE_$MB <- ifelse(SE_$schulart=="MB",1,0)
SE_$GS <- ifelse(SE_$schulart=="GS",1,0)
SE_$TR_SPF_r <- as.numeric(SE_$TR_SPF_r)
SE_$trnotedeu_r <- as.numeric(SE_$trnotedeu_r)
SE_$TR_geschlecht <- ifelse(SE_$TR_geschlecht == "männlich", 0,1)
table(as.integer(factor(SE_$Eabildm,levels=c("NIX", "AUS", "FS", "FHBA", "UNI"))))
SE_$Bilm <- as.integer(factor(SE_$Eabildm,levels=c("NIX", "AUS", "FS", "FHBA", "UNI")))
SE_$Bilv <- as.integer(factor(SE_$Eabildv,levels=c("NIX", "AUS", "FS", "FHBA", "UNI")))
SE_$zhg <- as.integer(SE_$zhg)
SE_$Sintne_b <- as.integer(SE_$Sintne_b)
SE_$Sintne_f <- as.integer(SE_$Sintne_f)
SE_$Sintne_i <- as.integer(SE_$Sintne_i)
SE_$Sintne_k <- as.integer(SE_$Sintne_k)
SE_$Pstudeu <- as.integer(SE_$Pstudeu)
table(SE_$Pstudeu, exclude=NULL)
SE_ <- SE_[,-which(colnames(SE_) %in% c("Eabildm", "Eabildv", "schulart", "System_Klass4_FDZ", "STUDY_TYPE"))]

# Start imputation
predM <- mice::make.predictorMatrix(data=SE_)
impM <- mice::make.method(data=SE_)
predM1 <- predM
predM1[,"IDSCH_FDZ"] <- 0
predM1[,"IDSTUD_FDZ"] <- 0
predM1[,"idteach_d_FDZ1"] <- 0
predM1["IDSCH_FDZ",] <- 0
predM1["IDSTUD_FDZ",] <- 0
predM1["idteach_d_FDZ1",] <- 0
predM1[c("Bilm", "Bilv", "zhg", "TR_SPF_r", "BEFKIwle", "trnotedeu_r", "wle_lesen", "wle_ortho", "wle_hoeren", 
         "wle_reading", "wle_listening", "CMC", "Sintne_b", "Sintne_f", "Sintne_i", "Sintne_k"),"IDSCH_FDZ"]  <- -2
predM1[c("Pstu9jg_FDZ", "Pstudeu", "Pewsort", "Pgtb", "Pspfges"),"IDSCH_FDZ"]  <- -2
impM1 <- impM
impM1["idteach_d_FDZ1"] <- ""
impM1["Bilm"] <- "2l.pmm"
impM1["Bilv"] <- "2l.pmm"
impM1["zhg"] <- "2l.pmm"
impM1["TR_SPF_r"] <- "2l.pmm" 
impM1["BEFKIwle"] <- "2l.pmm"
impM1["trnotedeu_r"] <- "2l.pmm"
impM1["wle_lesen"] <- "2l.pmm"
impM1["wle_ortho"] <- "2l.pmm"
impM1["wle_hoeren"] <- "2l.pmm"
impM1["wle_reading"] <- "2l.pmm"
impM1["wle_listening"] <- "2l.pmm"
impM1["CMC"] <- "2l.pmm"
impM1["Sintne_b"] <- "2l.pmm"
impM1["Sintne_f"] <- "2l.pmm"
impM1["Sintne_i"] <- "2l.pmm"
impM1["Sintne_k"] <- "2l.pmm"
impM1["Pstu9jg_FDZ"] <- "2lonly.function"
impM1["Pstudeu"] <- "2lonly.function"
impM1["Pewsort"] <- "2lonly.function"
impM1["Pgtb"] <- "2lonly.function"
impM1["Pspfges"] <- "2lonly.function"
imputationFunction <- list("Pstu9jg_FDZ"="pmm5", "Pstudeu"="pmm5", "Pewsort"="pmm5", "Pgtb"="pmm5", "Pspfges"="pmm5")
cluster_var <- list("Pstu9jg_FDZ"="IDSCH_FDZ", "Pstudeu"="IDSCH_FDZ", "Pewsort"="IDSCH_FDZ", "Pgtb"="IDSCH_FDZ", "Pspfges"="IDSCH_FDZ")

imp1 <- mice::mice(SE_ , m=20, predictorMatrix=predM1, method=impM1, maxit=30,
                    imputationFunction=imputationFunction, cluster_var=cluster_var, seed=987)
imp1$loggedEvents # passt, dependencies removed during imputation

nS <- length(unique(SE_$IDSCH_FDZ))
bn <- 200
doBoot <- function(y,x,w, dat){
  makeTheBoot <- function(it){
    ss <- dat[sample(size=nS, x=1:nS, replace=TRUE),]
    co <- weightedCorr(y= ss[,y], x=ss[,x], method="Polyserial", weights=ss[,w])
    return(co)
  }
  res <- sapply(1:bn, makeTheBoot)
  return(var(res))
}

CRES <- NULL
for(mm in 1:imp1$m){
  cat("IT: ",mm, "\n")
  IT <- complete(imp1, action=mm)
  #c1 <- polycor::polyserial(as.numeric(SF_$trnotedeu_r), as.numeric(SF_$wle_lesen), std.err = TRUE)
  c1 <- weightedCorr(y= as.numeric(IT$trnotedeu_r), x=as.numeric(IT$wle_lesen), method="Polyserial", weights=IT$totwgt_deu)
  v1 <- doBoot(y="trnotedeu_r", x="wle_lesen", w="totwgt_deu", dat=IT)
  #c2 <- polycor::polyserial(as.numeric(SF_$trnotedeu_r), as.numeric(SF_$wle_ortho), std.err = TRUE)
  c2 <- weightedCorr(y= as.numeric(IT$trnotedeu_r), x=as.numeric(IT$wle_ortho), method="Polyserial", weights=IT$totwgt_deu)
  v2 <- doBoot(y="trnotedeu_r", x="wle_ortho", w="totwgt_deu", dat=IT) 
  #c3 <- polycor::polyserial(as.numeric(SF_$trnotedeu_r), as.numeric(SF_$wle_hoeren), std.err = TRUE)
  c3 <- weightedCorr(y= as.numeric(IT$trnotedeu_r), x=as.numeric(IT$wle_hoeren), method="Polyserial", weights=IT$totwgt_deu)
  v3 <- doBoot(y="trnotedeu_r", x="wle_hoeren", w="totwgt_deu", dat=IT)
  #c4 <- polycor::polyserial(as.numeric(SF_$trnotedeu_r), as.numeric(SF_$wle_listening), std.err = TRUE)
  c4 <- weightedCorr(y= as.numeric(IT$trnotedeu_r), x=as.numeric(IT$wle_listening), method="Polyserial", weights=IT$totwgt_deu)
  v4 <- doBoot(y="trnotedeu_r", x="wle_listening", w="totwgt_deu", dat=IT)
  #c5 <- polycor::polyserial(as.numeric(SF_$trnotedeu_r), as.numeric(SF_$wle_reading), std.err = TRUE)
  c5 <- weightedCorr(y= as.numeric(IT$trnotedeu_r), x=as.numeric(IT$wle_reading), method="Polyserial", weights=IT$totwgt_deu)
  v5 <- doBoot(y="trnotedeu_r", x="wle_reading", w="totwgt_deu", dat=IT)
  CRES <- rbind(CRES, cbind(mm, c1, v1), cbind(mm, c2, v2), cbind(mm, c3, v3), cbind(mm, c4, v4), cbind(mm, c5, v5))
}
# combining rules
allC <- NULL
for(k in 1:5){
  el <- NULL
  for(j in 1:imp1$m){
    el <- rbind(el, CRES[CRES[,1]==j,][k,]) 
  }
  estC <- mean(el[,2])
  uC <- mean(el[,3])
  bC <- 1/(imp1$m-1)*sum((el[,2]-estC)^2)
  varC <- uC + (1+1/imp1$m)*bC
  allC <- rbind(allC, c(estC, varC)) 
}
alpha <- 0.05
ci_l <- allC[,1] - qt(1-alpha/2, nrow(SE_))*sqrt(allC[,2])
ci_u <- allC[,1] + qt(1-alpha/2, nrow(SE_))*sqrt(allC[,2]) 
Mj <- cbind(ci_l, allC[,1], ci_u)
colnames(Mj) <- c("lower CI", "estim", "upper CI")

par( mar=c(5, 10, 4, 0) )                                    
plot(Mj[1,c(1,3)], c(1,1), type="l", xlim=c(-0.5,-0.1), ylim=c(0.5,6.5), 
     xlab="polyserielle Korrelation", ylab="", axes=F, col="red", main="")
axis(1, at=c(-0.5, -0.4, -0.3, -0.2, -0.1), labels=c(-0.5, -0.4, -0.3, -0.2, -0.1))
labe <- c("","Lesekompetenz", "Orthog.komp.",  "Komp. Hoeren", "Komp. Engl. Hören", "Komp. Engl. Lesen","")
axis(2, at=0:6, labels=labe, las=2)
points(Mj[1,2],1, pch=20, col="red")
lines(Mj[2,c(1,3)], c(2,2), col="red")
points(Mj[2,2],2, pch=20, col="red")
lines(Mj[3,c(1,3)], c(3,3), col="red")
points(Mj[3,2],3, pch=20, col="red")
lines(Mj[4,c(1,3)], c(4,4), col="red")
points(Mj[4,2],4, pch=20, col="red")
lines(Mj[5,c(1,3)], c(5,5), col="red")
points(Mj[5,2],5, pch=20, col="red")

# schid <- unique(SE_$IDSCH_FDZ)
# for(i in 1:length(schid)){
#   if(i %in% seq(from=10, to=1500, by=100)){
#     cat("ID: ", i,"\n")
#   }
#   ll <- length(unique(SE_[SE_$IDSCH_FDZ %in% schid[i],"totwgt_deu"]))
#   if(ll>1){
#     cat("SCH: ", schid[i], "-- ll: ", ll, "-- Schulart: ", unique(SE_[SE_$IDSCH_FDZ %in% schid[i],"schulart"]), " -- FOE: ", SE_[SE_$IDSCH_FDZ %in% schid[i],"TR_SPF_r"], "\n")
#   }
# } # okay: alles Schulen mit zwei Fallgewichten sind entweder FOE (ein Gewicht für "normale FOE", ein für Überziehung) oder Schulen mit Kindern mit SPF (d.h. ein Gewicht für "normale Kinder" und eines für die mit SPF)
# 
# # -> Gewichte: zwei Arten je Schule: Schüler mit (aus Oversampling) / ohne SPF
# # -> Gewichte aus FOE Schulen können beiseite gelassen werden, da Stratifizierung nach Schulformen
# # -> Brauche NR-Adjustierung auf Daten mit D-Lehrerinfo: von N=31594 auf N=23321

### take imp. data sets, one after the other 
for(mm in 1:imp1$m){
  IT <- complete(imp1, action=mm)
  IT$mig <- ifelse(IT$zhg == 0, 0, 1)
  AA <- aggregate(IT$mig, list(SCH=IT$IDSCH_FDZ), mean) 
  table(AA$x)
  colnames(AA)[2] <- "AntMig"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH") # Beachte, hier sind SPF SuS aus der gesamten Jahrgangsstufe enthalten, d.h. Anteil betrifft einerseits alle Kinder in der Klasse ohne SPF Lernen & emot. Entwickl und dann alle Kinder mit SPF Lernen & emot. Entwicklung in der gesamten Jahrgangsstufe
  AA <- aggregate(IT$wle_lesen, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "wle_LesM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  AA <- aggregate(IT$wle_hoeren, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "wle_HoerM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  AA <- aggregate(IT$wle_ortho, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "wle_orthoM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  AA <- aggregate(IT$wle_reading, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "wle_readM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  AA <- aggregate(IT$wle_listening, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "wle_listM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  AA <- aggregate(IT$CMC, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "cmcM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  AA <- aggregate(IT$Bilm, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "bildmM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  AA <- aggregate(IT$Bilv, list(SCH=IT$IDSCH_FDZ), mean) 
  #table(AA$x)
  colnames(AA)[2] <- "bildvM"
  IT <- merge(IT, AA, by.x="IDSCH_FDZ", by.y="SCH")
  # Faktoranalyse für Anstrenungsbereitschaft SuS 
  items <- IT[, c("Sintne_b", "Sintne_f", "Sintne_i", "Sintne_k")]
  #fa.parallel(items)
  AnstrB <- fa(items,nfactors=1,rotate="varimax",scores=TRUE,fm="minres")
    #print(AnstrB,sort=TRUE,digits=2,cutoff=0.01)
    #plot(AnstrB)
  IT <-  IT[,-which(colnames(IT) %in% c("Sintne_b", "Sintne_f", "Sintne_i", "Sintne_k"))]
  IT <- cbind(IT,AnstrB$scores) 
  colnames(IT)[ncol(IT)] <- "AnstrB"
  
  SL <- merge(IT, L, by.x ="idteach_d_FDZ1", by.y="idteach_FDZ") # N=23345 von 31594 SuS, d.h. 8249 SuS ohne Lehrerinfo (entspricht 26,1% der SuS)
  #length(unique(SL$idteach_d_FDZ1)) # für 1051 Lehrer Infos
  #length(unique(SL[is.na(SL$Lgender),"idteach_d_FDZ1"])) # 36 Lehrer (N=739 SuS)
  #length(unique(SL[is.na(SL$Lage_FDZ),"idteach_d_FDZ1"])) # 51 Lehrer (N=1076 SuS)
  #length(unique(SL[is.na(SL$Llja_FDZ),"idteach_d_FDZ1"]))  # 52 Lehrer (N=1084 SuS)
  #table(SL$Lquest, exclude=NULL) # passt alle Lehrer in diesem Datensatz haben Fragebogen zu Deutsch ausgefüllt
  #table(SL$Llbfb_a) # vollkommen unklar, warum xx Lehrer (xx), die den Fragebogen zu Deutsch ausfüllen, hier nicht ankreuzen, dass sie derzeit Deutsch unterrichten
  SL <- SL[,-which(colnames(SL) %in% c("IDSCH_FDZ.y", "Llbfb_a", "Lquest"))]
  NN1 <- unique(SL[SL$Llja_FDZ<0,"idteach_d_FDZ1"]) # werfe Datenzeilen mit fehlender Lehrerinfo raus
  NN2 <- unique(SL[SL$Lgender<0,"idteach_d_FDZ1"])
  NN3 <- unique(SL[SL$Lage_FDZ<0,"idteach_d_FDZ1"])
  #length(intersect(NN1, NN2)) # 33 von 52 bzw. 36 gleich NA
  #length(intersect(NN1, NN3)) # 42 von 52 bzw. 51 gleich NA
  #length(intersect(NN2, NN3)) # 35 von 36 bzw. 51 gleich NA
  #length(unique(c(NN1,NN2, NN3))) # Schließe 62 Lehrer (5,9%) wg. miss Infos aus. Verbleiben N=22034 SuS, verliere somit 1311 SuS (5,6%).
  SL_ <- SL[!(SL$idteach_d_FDZ1 %in% unique(c(NN1,NN2, NN3))),]
  
  # Rechne Gewichte aus für SE_ (bzw. IT) auf SL_
  IT$Linfo <- ifelse(IT$idteach_d_FDZ1 %in% SL_$idteach_d_FDZ1, 1, 0) # N=9560 Fälle gehen verloren
  modSel <- glm(Linfo ~  Pstu9jg_FDZ + as.factor(Pstudeu) + as.factor(Pewsort) + Pgtb + Pspfges 
        + SuS_KL + AntMaed_KL + SysMod + SysMTMOD + RS + HS + MB + GS 
        + wle_LesM + wle_HoerM + wle_orthoM + wle_readM + wle_listM + cmcM 
        + bildmM + bildvM, family= binomial(link="logit"),dat=IT)
  pL <- predict(modSel, type="response")
  IT$pL <- pL 
  IT$wgt_L <- IT$totwgt_deu*(1/IT$pL)
  #table(round(IT$wgt_L))
  
  IT <- merge(IT, S[, c("IDSTUD_FDZ", "schulart", "System_Klass4_FDZ")], by="IDSTUD_FDZ")
  #tab <- round(wtd.table(IT$schulart, IT$System_Klass4_FDZ, weights=IT$wgt_L))
  IT_ <- merge(SL_, IT[, c("IDSTUD_FDZ", "Linfo", "wgt_L", "schulart", "System_Klass4_FDZ")], by="IDSTUD_FDZ")
  #table(round(IT_$wgt_L))
  
  # # überprüfe, ob ich alle Kinder aus einer Klasse habe
  # uS <- unique(IT_$IDSCH_FDZ.x)
  # for(ss in uS){
  #   #ss <- 1
  #   uu <- unique(IT_[IT_$IDSCH_FDZ.x %in% ss, "idteach_d_FDZ1"])
  #   cat("SchuID: ",ss, " -- LE: ", length(uu), " -- allL: ", uu, "\n")
  # } # passt ein Lehrer pro Schule: nur ein Klasse pro Schule
  # 
  # # gibt es kinder aus den Klassen, bei denen Schüler Lehrern zugeordnet werden, die nicht zugeordnet werden können
  # # -> Schaue mir an, welche SuS in Schulen, mit zugeordneten SuS, nicht zugeordnet werden können, falls das nur SuS mit SPF sind, dann schließe ich, dass diese SuS aus anderen Klassen der Jahrgangstufe kommen
  # xx <- IT[IT$IDSCH_FDZ %in% IT_$IDSCH_FDZ.x, ]
  # xy <- xx[is.na(xx$idteach_d_FDZ1),]
  # table(xy$IDSCH_FDZ, xy$TR_SPF_r) # in der Tat: nur SuS mit SPF -> gehe davon aus, dass Schulklasse komplett ist: erstelle Aggregate erneut
  
  IT_ <- IT_[,-which(colnames(IT_) %in% c("SuS_KL", "AntMaed_KL", "AntMig", "wle_LesM", "wle_HoerM", "wle_orthoM", "Swle_readM", "wle_listM", "cmcM", "bildmM", "bildvM"))]
  
  AA <- aggregate(IT_$IDSTUD_FDZ, list(SCH=IT_$IDSCH_FDZ.x), length) 
  colnames(AA)[2] <- "SuS_KL"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(as.numeric(IT_$TR_geschlecht), list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "AntMaed_KL"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")  
  AA <- aggregate(IT_$mig, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "AntMig"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH") # Beachte, hier sind SPF SuS aus der gesamten Jahrgangsstufe enthalten, d.h. Anteil betrifft einerseits alle Kinder in der Klasse ohne SPF Lernen & emot. Entwickl und dann alle Kinder mit SPF Lernen & emot. Entwicklung in der gesamten Jahrgangsstufe
  AA <- aggregate(IT_$wle_lesen, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "wle_LesM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(IT_$wle_hoeren, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "wle_HoerM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(IT_$wle_ortho, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "wle_orthoM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(IT_$wle_reading, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "wle_readM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(IT_$wle_listening, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "wle_listM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(IT_$CMC, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "cmcM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(IT_$Bilm, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "bildmM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")
  AA <- aggregate(IT_$Bilv, list(SCH=IT_$IDSCH_FDZ.x), mean) 
  colnames(AA)[2] <- "bildvM"
  IT_ <- merge(IT_, AA, by.x="IDSCH_FDZ.x", by.y="SCH")  
  
  # ddata <- svydesign(id= ~IDSTUD_FDZ, weights=~wgt_L, data=IT_)
  # allStrata <- unique(cbind(IT_$schulart, IT_$System_Klass4_FDZ))
  # allPossStrata <- expand.grid(c("GY", "RS", "HS", "MB", "GS"), c("MOD", "MTMOD", "MTTR"))
  # diffStrata <- setdiff(apply(allPossStrata,1,paste, collapse=","), apply(allStrata,1,paste,collapse=",")) # alle Post-Strata befüllt
  # eB <- array(NA, dim=c(5,3), dimnames = list(schulart=c("GS", "GY", "HS", "MB", "RS"), System_Klass4_FDZ=c("MOD", "MTMOD", "MTTR")))
  # for(i in 1:5){
  #   for(j in 1:3){
  #     eB[i,j] <- tab[i,j]
  #   }
  # }
  # postSt <- rake(ddata, sample=list(~schulart+System_Klass4_FDZ),
  #                population=list(as.table(eB)),
  #                control=list(maxit=100))
  # calweights <- attributes(postSt$postStrata[[1]][[1]])$weights
  # IT_ <- cbind(IT_, calweights)
  #postB <- matrix(NA, nrow=5, ncol=3)
  #for(i in 1:5){
  #  for(j in 1:3){
  #    postB[i,j] <- sum(IT_[IT_$schulart==rownames(eB)[i] & IT_$System_Klass4_FDZ==colnames(eB)[j], "calweights"])
  #  }
  #} # passt
  
  #head(IT_)
  IT_$GY <- ifelse(IT_$schulart=="GY",1,0)
  IT_$NIXm <- ifelse(IT_$Bilm == 1,1,0)
  IT_$AUSm <- ifelse(IT_$Bilm == 2,1,0)
  IT_$FSm <- ifelse(IT_$Bilm == 3,1,0)
  IT_$FHBAm <- ifelse(IT_$Bilm == 4,1,0)  
  IT_$UNIm <- ifelse(IT_$Bilm == 5,1,0)
  IT_$NIXv <- ifelse(IT_$Bilv == 1,1,0)
  IT_$AUSv <- ifelse(IT_$Bilv == 2,1,0)
  IT_$FSv <- ifelse(IT_$Bilv == 3,1,0)
  IT_$FHBAv <- ifelse(IT_$Bilv == 4,1,0)  
  IT_$UNIv <- ifelse(IT_$Bilv == 5,1,0)
  IT_$GebJ <- 1900 + round((IT_$CMC-1)/12)
  IT_$terBildm <- ifelse(IT_$Bilm %in% c(4,5),1,0)
  IT_$FB_diag1 <- ifelse(IT_$fort_diag==1, 1,0)
  IT_$FB_diag0 <- ifelse(IT_$fort_diag==0, 1,0) 
  IT_$FB_diag2 <- ifelse(IT_$fort_diag==2, 1,0)
  IT_$intGeschUni <- ifelse(IT_$TR_geschlecht == 1 & IT_$UNIm ==1, 1,0)
  IT_$intGeschFHBA <- ifelse(IT_$TR_geschlecht == 1 & IT_$FHBAm ==1, 1,0)
  IT_$intGeschUniFH <- ifelse(IT_$TR_geschlecht == 1 & IT_$terBildm ==1, 1,0)
  IT_$querFremdYes <- ifelse(IT_$querfremd==1,1,0)
  IT_$querFremdNo <- ifelse(IT_$querfremd==0,1,0)  
  IT_$querFremdNA <- ifelse(IT_$querfremd==2,1,0)
  IT_$Lgender <- as.numeric(IT_$Lgender)
  
  setwd("F:\\IQB_Paper\\Projekt_LehrerNoten\\Results")
  write.dta(IT_,paste("impData",mm,".dta",sep=""))
}











