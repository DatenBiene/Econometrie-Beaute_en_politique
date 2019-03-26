# Projet_Econom-trie2
Projet within the ENSAE second year engineering program

##
Notre Google doc de suivit de projet : 

##
Notre Overleaf : 
### Brève Timeline 

Rendre le Vendredi 17 Mai à 18H

### Ligne de conduite

Ne JAMAIS travailler sur master directement.

La bonne pratique de travail est la suivante : 

#### Pull master 

Afin d'être a jour avec le travail des autres.

```
git checkout master
git pull
```

#### Création nouvelle branche

(commencer le nom de votre branch avec votre prénom, il faut que le nom dela branche soit explicite)
Ne créer qu'un ou deux documents sur cette branche (pas plus sinon faites une autre branche), cela facilite le travail de relecture. 

- Un fois le travail terminé (ou que le travail est significatif et utile pour les autres) pusher votre branche (voir section ci-dessus "Edit and push code on your branch")

- Puis sur Github faite une PULL REQUEST (aller sur la branche et demander pull request) : dans la description soyez très explicite sur ce que vous avez fait et en quoi c'est utile.

- Après une peer review on merge cette branche dans master

#### Supression branche

-Quand votre branche a été mergée vous devez supprimer votre branche sur le GitHub:  mettez vous sur une autre branche (avec git checkout) et utilisez :

```
git push origin --delete {Nom de la branche}
```
 Pour supprimer en local (sur votre ordi) :
 
```
git branch -d {Nom de la branche}
```

Important : Dans une pull request il doit y avoir 1 ou deux documents modifiés et pas plus ! Sinon le travail de review deviens illisible.

Ne pas oublier de faire de bons noms de commit et de pull request (sinon Git perd tout son interet de lisibilité d'évolution du projet)


#### Idées pour avancer 

Méthodes générales 
https://cs.stackexchange.com/questions/61102/detecting-events-in-time-series-data
https://stackoverflow.com/questions/12851208/how-to-detect-significant-change-trend-in-a-time-series-data

Deep Learning
https://www.infoq.com/articles/deep-learning-time-series-anomaly-detection
