setwd('C:/Users/User/Desktop/Econométrie 2')

data <- read.csv('data.csv', header = TRUE, sep=";",dec = ',')
n = dim(data)[1]
p = dim(data)[2]

#Question 2  corrélation entre les évaluateurs
x_eval = data[,16:19]    #variables des évaluateurs
cor = cor(x_eval, method = c("pearson", "kendall", "spearman"))

#Question 3 moyenne et variance
moyenne_im24 = mean(as.numeric(x_eval[,1]))
moyenne_if39 = mean(as.numeric(x_eval[,2]))
moyenne_nif24 = mean(as.numeric(x_eval[,3]))
moyenne_nim40 = mean(as.numeric(x_eval[,4]))
variance_im24 = mean(as.numeric(x_eval[,1])^2)-mean(as.numeric(x_eval[,1]))^2
variance_if39 = mean(as.numeric(x_eval[,2])^2)-mean(as.numeric(x_eval[,2]))^2
variance_nif24= mean(as.numeric(x_eval[,3])^2)-mean(as.numeric(x_eval[,3]))^2
variance_nim40 = mean(as.numeric(x_eval[,4])^2)-mean(as.numeric(x_eval[,4]))^2
moyennes = c(moyenne_if39,moyenne_im24,moyenne_nif24,moyenne_nim40)
variances = c(variance_if39,variance_im24,variance_nif24,variance_nim40)

g_range <- range(0, moyennes, variances)
plot(moyennes, type="o", col="blue", ylim=g_range+1.5, 
     axes=FALSE, ann=FALSE)
lines(variances, type="o", pch=22, lty=2, col="red")
axis(1, at=1:4, lab=c("if39","im24","nif24","nim40"))
axis(2, las=1, at=4*0:g_range[2])
box()
title(xlab="Evaluateurs", col.lab=rgb(0,0.5,0))
title(ylab="Valeurs", col.lab=rgb(0,0.5,0))
title(main="Moyenne et Variance", font.main=4)
legend(1, g_range[2]+1.5, c("moyennes","variances"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

#Normalisation

norm_if39 = (x_eval[,1]-moyenne_if39)/sqrt(variance_if39)
norm_im24 = (x_eval[,2]-moyenne_im24)/sqrt(variance_im24)
norm_nif24= (x_eval[,3]-moyenne_nif24)/sqrt(variance_nif24)
norm_nim40 = (x_eval[,4]-moyenne_nim40)/sqrt(variance_nim40)

z_score = data.frame(norm_if39=norm_if39,norm_im24=norm_im24,norm_nif24=norm_nif24,norm_nim40=norm_nim40)

#Question 4: Statistiques descriptives des variables pertinentes

female = data[,'female']
skin = data[,'skin']
forehead = data[,'forehead']
nose = data[,'nose']
chin = data[,'chin']
par(mfrow = c(2,2))
hist(chin)
hist(skin)
hist(forehead)
hist(nose)

#effet de la couleur de la peau sur les notes
agg = aggregate(x_eval, list(data$skin), mean)
autochtone = agg[,2]+agg[,3]
nonautochtone = agg[,4]+agg[,5]
g_range <- range(0, autochtone, nonautochtone)
plot(autochtone, type="o", col="blue", ylim=g_range+1.5, 
     axes=FALSE, ann=FALSE)
lines(nonautochtone, type="o", pch=22, lty=2, col="red")
axis(1, at=1:11)
axis(2, las=1, at=4*0:g_range[2])
box()
title(xlab="skin", col.lab=rgb(0,0.5,0))
title(ylab="somme_moyenne", col.lab=rgb(0,0.5,0))
title(main="influence couleur de peau sur score", font.main=4)
legend(1, g_range[2]+1.5, c("autochtone","nonautochtone"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

#effet du sexe sur la notation

agg_sexe = aggregate(x_eval, list(data$female), mean)
moy_homme = agg_sexe[,2]+agg_sexe[,5]
moy_femme = agg_sexe[,3]+agg_sexe[,4]
par(mfrow = c(1,2))
barplot(moy_homme,xlab = "homme/femme",ylab="somme des moyennes")
title(main = "Noteurs hommes")
barplot(moy_femme,xlab = "homme/femme",ylab="sommes des moyennes")
title(main = "Noteurs femmes")

#Question 5 : Part de vote obtenus en fonction du score de beauté

#Création de la variable mean_score (moyenne des scores des 4 noteurs)
mean_score = rowSums(z_score)/dim(z_score)[1]
data=data.frame(data,mean_score)    #ajout de la nouvelle variable au dataframe
data$voteshare = as.numeric(data$voteshare)
data$mean_score=as.numeric(data$mean_score)

#Regression de voteshare sur mean_score
reg = lm(voteshare~mean_score,data = data)

#On affiche la regression
plot(x = mean_score,y = data[,'voteshare'],xlab = "score moyen", ylab = "voteshare")
title(main = "voteshare en fonction du score moyen")
abline(reg$coefficients[1],reg$coefficients[2])

#Séparation candidat sortant et opposant avec la variable incumbentcandidate (=1 si sortant)

data_sortant = subset(data,incumbentcandidate==1)
data_opposant = subset(data,incumbentcandidate==0)

#Régressions
reg1 = lm(voteshare~mean_score,data = data_sortant)
reg2 = lm(voteshare~mean_score,data = data_opposant)

#On affiche les regressions
par(mfrow = c(1,2))
#Candidats sortants
plot(x = data_sortant[,'mean_score'],y = data_sortant[,'voteshare'],xlab = "score moyen", ylab = "voteshare")
title(main = "Candidats sortants")
abline(reg1$coefficients[1],reg1$coefficients[2])

#Candidats opposants

plot(x = data_opposant[,'mean_score'],y = data_opposant[,'voteshare'],xlab = "score moyen", ylab = "voteshare")
title(main = "Candidats opposants")
abline(reg2$coefficients[1],reg2$coefficients[2])

#iii)

reg = lm(voteshare~skin,data = data)
reg_sortant = lm(voteshare~skin,data = data_sortant)
reg_opposant = lm(voteshare~skin,data = data_opposant)

par(mfrow = c(1,3))
#Tous les candidats
plot(x = data[,'skin'],y = data[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Tous les candidats")
abline(reg$coefficients[1],reg$coefficients[2])

#Candidats sortants
plot(x = data_sortant[,'skin'],y = data_sortant[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats sortants")
abline(reg_sortant$coefficients[1],reg_sortant$coefficients[2])

#Candidats opposants

plot(x = data_opposant[,'skin'],y = data_opposant[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats opposants")
abline(reg_opposant$coefficients[1],reg_opposant$coefficients[2])

#iv)

ab = data[,'ab']
ab_sort = sort(ab)
#On fait un groupe 
data_low_ab = subset(data,ab<0.134)
data_high_ab = subset(data,ab>=0.134)

data_low_sortant = subset(data_low_ab,incumbentcandidate==1)
data_low_opposant = subset(data_low_ab,incumbentcandidate==0)
data_high_sortant = subset(data_high_ab,incumbentcandidate==1)
data_high_opposant = subset(data_high_ab,incumbentcandidate==0)

#Groupe low_ab

reg = lm(voteshare~skin,data = data_low_ab)
reg_sortant = lm(voteshare~skin,data = data_low_sortant)
reg_opposant = lm(voteshare~skin,data = data_low_opposant)

par(mfrow = c(1,3))
#Tous les candidats low ab
plot(x = data_low_ab[,'skin'],y = data_low_ab[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats faible ab")
abline(reg$coefficients[1],reg$coefficients[2])

#Candidats sortants low ab
plot(x = data_low_sortant[,'skin'],y = data_low_sortant[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats sortants faible ab")
abline(reg_sortant$coefficients[1],reg_sortant$coefficients[2])

#Candidats opposants low ab

plot(x = data_low_opposant[,'skin'],y = data_low_opposant[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats opposants faible ab")
abline(reg_opposant$coefficients[1],reg_opposant$coefficients[2])

#Groupe high_ab

reg = lm(voteshare~skin,data = data_high_ab)
reg_sortant = lm(voteshare~skin,data = data_high_sortant)
reg_opposant = lm(voteshare~skin,data = data_high_opposant)

par(mfrow = c(1,3))
#Tous les candidats high ab
plot(x = data_high_ab[,'skin'],y = data_high_ab[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats fort ab")
abline(reg$coefficients[1],reg$coefficients[2])

#Candidats sortants high ab
plot(x = data_high_sortant[,'skin'],y = data_high_sortant[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats sortants fort ab")
abline(reg_sortant$coefficients[1],reg_sortant$coefficients[2])

#Candidats opposants high ab

plot(x = data_high_opposant[,'skin'],y = data_high_opposant[,'voteshare'],xlab = "skin", ylab = "voteshare")
title(main = "Candidats opposants fort ab")
abline(reg_opposant$coefficients[1],reg_opposant$coefficients[2])

#Question 7
#i)
#Modèle 1: moyenne des notes
data['true_voteshare'] = data['voteshare']*(1-0.0375)
data['Y'] = log(data['true_voteshare']/0.0375,2)

reg = lm(Y~mean_score-1,data = data)
summary(reg)

#Modèle 2: moyenne pondéré des notes

data['note_pond'] = data['ab']*(data['im24']+data['if39'])/2+(1-data['ab'])*(data['nif24']+data['nim40'])/2

reg = lm(Y~note_pond-1,data = data)
summary(reg)

#Modèle 3: Incumbentcandidate

reg = lm(Y~note_pond+note_pond*incumbentcandidate-1,data = data)
summary(reg)

#Modèle 4: skin

reg = lm(Y~skin-1,data = data)
summary(reg)

#Modèle 5: skin et ab
#On reprend les deux groupes faient avant: ab<0.134 et ab>0.134
#On code la fonction indicatrice
indicatrice <- function(x,a)  ifelse(x >= a, 1,0)   # =1 si haut % indigène
data['ind_ab'] = indicatrice(data['ab'],0.134)
reg = lm(Y~skin*ind_ab-1,data = data)
summary(reg)

