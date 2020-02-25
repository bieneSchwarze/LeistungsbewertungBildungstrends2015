####################################################################
###
### AUSWERTUNG IMPUTIERTE DATENSAETZE IQB BILDUNGSTREND PROJEKT
### 07.03.2019
### Sabine
###
####################################################################
####################################################################

library("ggplot2")
library("gridExtra")

# Kombiniere Ergebnisse aus imputierten Datensaetzen (oprobit, gllamm)
setwd("F:\\SZ\\Projekt_LehrerNoten\\Results")
A <- read.table("resAll_m.txt", sep="\t", fill=TRUE, dec=",")
head(A)
colnames(A) <- c("Coef.", "Std. Err.", "z", "P>|z|", "95% UC", "95% OC")
N <- 21813
m <- 27
nc <- 23 
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
rownames(Mj) <- c("KompOrtho", "Log.Schlussf", "AnstrBer", "Maedchen", "SPF_ja", "MutterFHBA", "MutterUni", "MigH_ja",
                  "Gymn", "AnzSPFSchule", "AnzSuSKl", "DurchschnittlKompet" ,"AntMig", "AntMaedKl", "BildSysMod", "LehrerWeibl",
                  "LehrerErfJahre", "QuerFremdNo", "MaedchenMutterFHBA", "MaedchenMutterUni", "KompetMutterFHBA",
                  "KompetMuttterUni", "LehrerinMaedchen")
res <- data.frame(Variable = rownames(Mj),
                  Koeffizient = Mj[,2],
                  LCI = Mj[,1],
                  UCI=Mj[,3])
res[,1] <- factor(res[,1], levels=rev(rownames(Mj)))
zp1 <- ggplot(res)
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = LCI, ymax = UCI),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Koeffizient, ymin = LCI, ymax = UCI),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle("Ordered Probit Regression (gllamm)")
#print(zp1) 

# Kombiniere Ergebnisse aus imputierten Datensaetzen (ologit, meologit)
setwd("F:\\SZ\\Projekt_LehrerNoten\\Results")
A <- read.table("resLogit.txt", sep="\t", fill=TRUE, dec=",")
head(A)
colnames(A) <- c("Coef.", "Std. Err.", "z", "P>|z|", "95% UC", "95% OC")
N <- 21813
m <- 30
nc <- 23 
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
rownames(Mj) <- c("KompOrtho", "Log.Schlussf", "AnstrBer", "Maedchen", "SPF_ja", "MutterFHBA", "MutterUni", "MigH_ja",
                  "Gymn", "AnzSPFSchule", "AnzSuSKl", "DurchschnittlKompet" ,"AntMig", "AntMaedKl", "BildSysMod", "LehrerWeibl",
                  "LehrerErfJahre", "QuerFremdNo", "MaedchenMutterFHBA", "MaedchenMutterUni", "KompetMutterFHBA",
                  "KompetMuttterUni", "LehrerinMaedchen")
res <- data.frame(Variable = rownames(Mj),
                  Koeffizient = Mj[,2],
                  LCI = Mj[,1],
                  UCI=Mj[,3])
res[,1] <- factor(res[,1], levels=rev(rownames(Mj)))
zp2 <- ggplot(res)
zp2 <- zp2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp2 <- zp2 + geom_linerange(aes(x = Variable, ymin = LCI, ymax = UCI),
                            lwd = 1, position = position_dodge(width = 1/2))
zp2 <- zp2 + geom_pointrange(aes(x = Variable, y = Koeffizient, ymin = LCI, ymax = UCI),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp2 <- zp2 + coord_flip() + theme_bw()
zp2 <- zp2 + ggtitle("Ordered Logit Regression (meologit)")
#print(zp2) 
grid.arrange(zp1, zp2, nrow = 1)

# ICC (oprobit, gllamm)
B <- read.table("rE.txt", sep="\t", fill=TRUE, dec=",")
rE <- as.numeric(as.character(B[,1]))
hist(rE)
round(rE/(1+rE)*100,2)
range(round(rE/(1+rE)*100,2))
mean(round(rE/(1+rE)*100,2))
sqrt(var(round(rE/(1+rE)*100,2)))

# Pseudo R^2
B <- read.table("r2.txt", sep="\t", fill=TRUE, dec=",")
r2 <- as.numeric(as.character(B[,1]))
hist(r2)
range(round(r2,2))
mean(round(r2,2))
sqrt(var(round(r2,2)))
