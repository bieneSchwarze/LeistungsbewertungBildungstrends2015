####################################################################
###
### AUSWERTUNG IMPUTIERTE DATENSÄTZE IQB BILDUNGSTREND PROJEKT
### 26.11.2019
### Sabine Zinn
###
####################################################################
####################################################################

library("ggplot2")

setwd("F:\\IQB_Paper\\Projekt_LehrerNoten\\Results")
A <- read.table("modRES(2)E.txt", sep="\t", fill=TRUE, dec=",")
head(A)
N <- 22034
m <- 18# 20
nc <- 22 
allC <- NULL
for(k in 1:nc){
  el <- NULL
  AC <- A[seq(from=k, to=nrow(A), by=nc),]
  estC <- mean(AC[,2])
  uC <- mean(AC[,3]^2)
  bC <- 1/(m-1)*sum((AC[,2]-estC)^2)
  varC <- uC + (1+1/m)*bC
  allC <- rbind(allC, c(estC, varC)) 
}
alpha <- 0.05
ci_l <- allC[,1] - qt(1-alpha/2, N)*sqrt(allC[,2])
ci_u <- allC[,1] + qt(1-alpha/2, N)*sqrt(allC[,2]) 
Mj <- cbind(ci_l, allC[,1], ci_u)
colnames(Mj) <- c("lower CI", "estim", "upper CI")
rownames(Mj) <- A[1:nc,1]
Mj <- round(Mj,3)
rownames(Mj) <- c("KompOrtho", "Log.Schlussf", "AnstrBer", "Mädchen", "SPF_ja", "MutterUni", "MutterFA/Bach", "MigH_ja",
                  "Gymn", "AnzSPFSchule", "AnzSuSKl", "AntMig", "AntMädKl", "BildSysMod", "LehrerWeibl",
                  "LehrerErfJahre", "FortBDiagK", "FortBUnbek", "QuerFremdNo", "QuerFremdNA",  "MädchenMutterUni", "MädchenMutterFHBach")
#res <- data.frame(Variable = rownames(Mj),
#                  Koeffizient = Mj[,2],
#                  LCI = Mj[,1],
#                  UCI=Mj[,3])
#res[,1] <- factor(res[,1], levels=rev(rownames(Mj)))
#zp1 <- ggplot(res)
#zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
#zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = LCI, ymax = UCI),
#                            lwd = 1, position = position_dodge(width = 1/2))
#zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Koeffizient, ymin = LCI, ymax = UCI),
#                             lwd = 1/2, position = position_dodge(width = 1/2),
#                             shape = 21, fill = "WHITE")
#zp1 <- zp1 + coord_flip() + theme_bw()
#zp1 <- zp1 + ggtitle("")
#print(zp1) 
Mk <- Mj[c(1,2,3,4,5,8,6,7,21,22,11,10,12,13,9,14,16,15,17,18,19,20),]
rownames(Mk) <- c("Kompetenz Deutsch Orthografie", "BEFKI (kognitive Grundfähigkeit)", "Anstrengungsbereitschaft", 
                  "Mädchen (vs. Junge))", "SPF: ja (vs. nein)", "Migrationshintergrund: ja (vs. nein)", 
                  "Mutter hat Universitätsabschluss: ja (vs. nein)", "Mutter hat Bachelor-/Fachhochschulabschluss: ja (vs. nein)",
                  "Mutter Mädchen hat Universitätsabschluss", "Mutter Mädchen hat Bachelor-/Fachhochschulabschluss",
                  "Anzahl Schüler/innen in Klasse", "Anzahl Schüler/innen mit SPF an Schule",
                  "Anteil Schüler/innen mit Migrationshintergrund in Klasse",
                  "Anteil Mädchen in Klasse", "Gymnasium (vs. andere Schulform)",
                  "Bildungssystem: modernisierte Strukturen (vs. nicht)",
                  "Erfahrung als Lehrkraft in Jahren", "Lehrkraft: Frau (vs. Mann)",
                  "Fortbildung: ja (vs. nein)", "Fortbildung: unbekannt (vs. nein)",
                  "Quereinstieg: nein (vs. ja)", "Quereinstieg: unbekannt (vs. ja)")
res <- data.frame(Variable = rownames(Mk),
                  Koeffizient = Mk[,2],
                  LCI = Mk[,1],
                  UCI=Mk[,3])
res[,1] <- factor(res[,1], levels=rev(rownames(Mk)))
cols <- c(rep(gray(0.30),6), rep(gray(0.40),6), rep(gray(0.15),10))
zp1 <- ggplot(res)
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = LCI, ymax = UCI), colour=cols,
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Koeffizient, ymin = LCI, ymax = UCI),
                             lwd = 0.8, position = position_dodge(width = 1/2),
                             colour=cols, shape = 21, fill = cols)
zp1 <- zp1 + coord_flip() + theme_bw()  
zp1 <- zp1 + ggtitle("") +  ylab("beta-Koeffizient") + xlab("") 
zp1 <- zp1 + theme(axis.text.y = element_text(face="bold", color=cols))
zp1 <- zp1 + theme(panel.background = element_rect(fill = "grey97"), panel.border = element_blank())
print(zp1)                   

