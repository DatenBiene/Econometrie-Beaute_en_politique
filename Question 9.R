#Question 9 
#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
  
setwd('C:/Users/Delanoue/Documents/ENSAE/S2-2A/Econométrie 2')

data <- read.csv('data.csv', header = TRUE, sep=';')

#Bon format variables 
data$ab=as.numeric(gsub(",", ".", data$ab))

#Creation Score de Beauté Pondéré 
norm_if39 = (data$if39-mean(data$if39))/sqrt(var(data$if39))
norm_im24 = (data$im24-mean(data$im24))/sqrt(var(data$im24))
norm_nif24= (data$nif24-mean(data$nif24))/sqrt(var(data$nif24))
norm_nim40 = (data$nim40-mean(data$nim40))/sqrt(var(data$nim40))

z_score = data.frame(norm_if39=norm_if39,norm_im24=norm_im24,norm_nif24=norm_nif24,norm_nim40=norm_nim40)

data$beaute_pond = data$ab*((z_score$norm_im24+z_score$norm_if39)/2)+(1-data$ab)*((z_score$norm_nim40+z_score$norm_nif24)/2)

library(stargazer)

model1 <- glm(winner ~beaute_pond+incumbentcandidate+female+ballotorder,family=binomial(link='logit'),data=data)
stargazer(model1)
summary(model1)

model2 <- glm(winner ~beaute_pond+incumbentcandidate+female+ballotorder+beaute_pond*incumbentcandidate,family=binomial(link='logit'),data=data)
stargazer(model2)

model3 <- glm(winner ~beaute_pond+incumbentcandidate+female+ballotorder+beaute_pond*female,family=binomial(link='logit'),data=data)
stargazer(model3)

model4 <- glm(winner ~skin+ab+incumbentcandidate+female+ballotorder,family=binomial(link='logit'),data=data)
stargazer(model4)

model5 <- glm(winner ~skin+ab+incumbentcandidate+female+ballotorder+skin*ab,family=binomial(link='logit'),data=data)
stargazer(model5)

model6 <- glm(winner ~beaute_pond+skin+ab+incumbentcandidate+female+ballotorder+skin*ab,family=binomial(link='logit'),data=data)
stargazer(model6)

#Question 11
#https://www.princeton.edu/~otorres/LogitR101.pdf

#install.packages("MASS")
library(MASS)

data$winner2<-as.factor(data$winner2)
multi1 <-polr(winner2 ~ beaute_pond+skin+ab+female+skin*ab+beaute_pond*female,data=data, Hess=TRUE)

stargazer(multi1)

multi2 <-polr(winner2 ~ beaute_pond+incumbentcandidate+female+ballotorder+beaute_pond*female,data=data, Hess=TRUE)
summary(multi2)
stargazer(multi2)

multi3 <-polr(winner2 ~ beaute_pond+incumbentcandidate+female+ballotorder+beaute_pond*female,data=data, Hess=TRUE)
summary(multi3)
stargazer(multi3)

