#setwd('C:/Users/Delanoue/Documents/ENSAE/S2-2A/Econométrie 2') #Pierre
setwd('C:/Users/User/Desktop/Econométrie 2') #Maxime

data <- read.csv('data.csv', header = TRUE, sep=';')

n = dim(data)[1]
p = dim(data)[2]


#Bon format variables 
data$ab=as.numeric(gsub(",", ".", data$ab))
data$voteshare = as.numeric(data$voteshare)
data$winner2<-as.factor(data$winner2)

#Creation Score de Beauté Pondéré 
norm_if39 = (data$if39-mean(data$if39))/sqrt(var(data$if39))
norm_im24 = (data$im24-mean(data$im24))/sqrt(var(data$im24))
norm_nif24= (data$nif24-mean(data$nif24))/sqrt(var(data$nif24))
norm_nim40 = (data$nim40-mean(data$nim40))/sqrt(var(data$nim40))

z_score = data.frame(norm_if39=norm_if39,norm_im24=norm_im24,norm_nif24=norm_nif24,norm_nim40=norm_nim40)

data$beaute_mean=as.numeric(rowMeans(z_score))
data$beaute_pond = data$ab*((z_score$norm_im24+z_score$norm_if39)/2)+(1-data$ab)*((z_score$norm_nim40+z_score$norm_nif24)/2)

data['true_voteshare'] = data['voteshare']*(1-0.0375)
data['Y'] = log(data['true_voteshare']/0.0375,2)
#Question 8

#ii)Test de corrélation entre note_pond et les variables physiologiques

#Choix instruments pour la beauté
reg_instru_beaute = lm(beaute_pond~forehead+chin+nose+incumbentcandidate+ballotorder+female, data = data)
stargazer(reg_instru_beaute)

#Test de Sargan
reg = lm(beaute_pond~forehead+chin+female+incumbentcandidate+ballotorder,data = data)
X = predict(object = reg, newdata = data)
data['instruments'] = X
reg_1 = lm(Y~instruments + incumbentcandidate + female + ballotorder ,data = data)
u = residuals(reg_1)
reg_u = lm(res~incumbentcandidate+ballotorder+female,data = data)
stargazer(reg_u)

#iii)
#On crée la variable instrumentale par la méthode 2SLS
reg = lm(beaute_pond~forehead+chin+female+incumbentcandidate+ballotorder,data = data)
X = predict(object = reg, newdata = data)
data$instruments = X

#On lance les modèles 1,2,3 et 6
reg_1 = lm(Y~instruments + incumbentcandidate + female + ballotorder ,data = data)

reg_2 = lm(Y~instruments+incumbentcandidate+female+ballotorder+instruments*incumbentcandidate,data = data)

reg_3 = lm(Y~instruments+incumbentcandidate+female+ballotorder+instruments*female,data = data)

reg_6 = lm(Y~instruments+skin*ab+skin+female+incumbentcandidate+ab+ballotorder,data = data)

#On récupère les résultats

stargazer(reg_1)
stargazer(reg_2)
stargazer(reg_3)
stargazer(reg_6)

#Question 10

#On trouve l'estimateur eta
reg_1 = lm(beaute_pond~ballotorder+incumbentcandidate+female+chin, data = data)
eta = residuals(reg_1)

#Modèle 1 à 3
probit_1 = glm(winner~ballotorder+female+incumbentcandidate + beaute_pond+eta,data = data,family = binomial(link='probit'))
stargazer(probit_1)

probit_2 = glm(winner~ballotorder+female+incumbentcandidate + beaute_pond + beaute_pond*incumbentcandidate + eta,data = data,family = binomial(link='probit'))
stargazer(probit_2)

probit_3 = glm(winner~ballotorder+female+incumbentcandidate + beaute_pond + beaute_pond*female + eta,data = data,family = binomial(link='probit'))
stargazer(probit_3)

#Modèle 4 et 5
#Choix de l'instrument pour skin
reg_instru_skin = lm(skin~forehead+nose+chin+ballotorder+female+incumbentcandidate+ab,data = data)
stargazer(reg_instru_skin)

#On trouve l'estimateur de eta2
reg_skin = lm(skin~ballotorder+incumbentcandidate+female+ab+chin, data = data)
eta2 = residuals(reg_skin)

probit_4 = glm(winner~ballotorder+female+incumbentcandidate + ab + skin + eta2,data = data,family = binomial(link='probit'))

probit_5 = glm(winner~ballotorder+female+incumbentcandidate + ab + skin + skin*ab + eta2,data = data,family = binomial(link='probit'))

probit_6 = glm(winner~ballotorder+female+incumbentcandidate + skin + ab + skin*ab + beaute_pond + eta,data = data,family = binomial(link='probit'))

stargazer(probit_4)
stargazer(probit_5)
stargazer(probit_6)



