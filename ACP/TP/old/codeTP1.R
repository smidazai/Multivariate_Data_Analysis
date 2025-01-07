#TP1 : Stat. des. et ACP 
#A faire au début de chaque séance:session-set working directory
#-choose directory pour relier le dossier anadon à RStudio



### Question 1 ----
#Importation du fichier pays_eu.txt
pays=read.table("pays_eu.txt",header=TRUE)#on peut aussi importer par les menus "Import Dataset")


#autre méthode
pays=read.table(file.choose(),header=TRUE)
help(read.table)
pays
head(pays)


#On change le nom des lignes (pour les grahiques de l'ACP)
pays=read.table(file.choose(),header=TRUE,row.names="PAYS")
#alternative 
paysbis=read.table("pays_eu.txt",header=TRUE,row.names=1)#la colonne PAYS est la première colonne du fichier


dim(pays)

pays2=pays[,2:12]#on enleve les variables qualitatives 
dim(pays2)


#Question 2 ---- 
# Statistique descriptive univariée
summary(pays2)
POP
pays2$POP#la variable POP

attach(pays2)#pour appeler les variables du fichier sans utiliser le $
POP

#ecart-type
sd(POP)#standard deviation
help(sd)

#Calul de tous les ecarts-type en meme temps
apply(pays2,2,sd)# tous les ecartypes
apply(pays2,2,mean)#toutes les moyennes

#Coef de variations (écart-type/moyenne) : seuil 0.25 (si CV est >0.25 variable dispersee autour de sa moyenne et variable peu dispersee si CV<0.25)
tabCV=apply(pays2,2,sd)/apply(pays2,2,mean)
tabCV

round(tabCV,digits=2)#arrondir

# histogramme de TACT
hist(TACT,freq=FALSE,col="green",main="Répartition des pays selon le taux d'activité",ylim=c(0,0.06))

# boite a moustaches de la variable TCHOM
boxplot(TCHOM,main="Boîte à moustaches du taux de chômage")
points(mean(TCHOM),col="cyan",pch="*",cex=2)#pour rajouter la moyenne à la boîte à moustaches

boxplot(pays2, ylim=c(0,40000))


#
#Statistique bivariée
# Nuages de points
plot(POP,TEL)

plot(pays2,main="Matrice des nuages de points")


#Question 3 ----- 
# Méthode 1 : Matrice des corrélations
cor(POP,TEL)
tabcor=cor(pays2)
round(tabcor,digits=2)

# Méthode 2 : Avec les packages ggplot2 et ggcorrplot 
#installer d'abord les deux packages
library(ggplot2)
library(ggcorrplot)
ggcorrplot(tabcor)
ggcorrplot(tabcor, hc.order = TRUE, lab = TRUE)
ggcorrplot(tabcor, hc.order = TRUE, type = "lower",lab = TRUE)

#comments : il existe des corrélations linéaires assez élevées en valeur absolue entre 
#plusieurs variables de départ, par exemple : 
#on peut remarquer que TCHOMLD est très corrolée linéairement positivement avec TCHOM, 
#TACT est corrélée linéairement négativement avec TCHOM et corrélée positivement avec TEMP, ..


#analyse bivariée -- 
# Nuage de points simples avec ggplot2
ggplot(pays2, aes(x=POP, y=TEL)) + geom_point()

# Changer la taille la couleur et la forme et mettre des labels
ggplot(pays2, aes(x=POP, y=TEL)) +
  geom_point(size=2,col="BLUE", shape=23)+geom_text(label=rownames(pays2))



####################################
#ACP avec FactoMineR sur le fichier
#des pays européens

# Question 4 ---- 
# ----- Etape 1 : corrélations entre les variables initiales ---

matcor=cor(pays2)
round(matcor,digits=2)#ou avec le package ggcorrplot (voir stat des-ci-dessus)
#il existe des corrélations linéaires assez élevées en valeur absolue entre les 
#variables de départ (les donner), ce qui justifie l'ACP

# Question 5 ----
#On fait l'ACP avec la fonction PCA()
resuacp=PCA(pays2)

# ------ Etape 2 : choix de la dimension -----

resuacp$eig
# 1. Critère de Kaiser : 3vp >1 mais lambda_4 = 0,97
# 2. Critère de la part d'inertie expliquée : 84% de l'inertie est conservée si l'on retient
  #les 4 premières composantes principales
# 3. Critère de la différence : on va tracer l'Eboulis des valeurs propres
   plot(resuacp$eig[,1])
   lines(resuacp$eig[,1])
   #Coude à la 3ème valeur propre donc on retiendrait les 2 premières cp avec ce critère
   
# Décision:k= 3 ou 4
help(PCA) #pour voir les différents arguments de la fonction PCA()
resuacp=PCA(pays2,ncp=4) #pour gader les résultats sur les 4 premières cp


#Question 6 ---- 
# ----- Etape 3 : Interprétation des cp retenues (corrélations et contributions)------
resuacp$var$cor
round(resuacp$var$cor,digits=2)
#C1 très corrélée linéairement >0 avec EVH,EVF,PIBH,TACT
#et <0 avec CHOM, CHOMLD
#C2 corrélée linéairement >0 avec POP et TEL


#C3 corrélée linéairement >0 avec TEMP et<0 avec TINF à partir de ça ? resuacp=PCA(pays2,ncp=4,axes=c(1,3))
#C4 corrélée linéairement >0 avec MARIAGE à partir de ça ? resuacp=PCA(pays2,ncp=4,axes=c(1,4))


# Contributions des variables initiales aux cp retenues
resuacp$var$contrib
round(resuacp$var$contrib,digits=2)
#seuil = 100/11 =9 (une forte contribution est une contribution supérieur à 100/n)
#mêmes conclusions que les cor
resuacp=PCA(pays2,ncp=4,axes=c(3,4))#pour avoir les graphiques sur les axes 3 et 4


#----- Etape 4 : qualité de représentation des pays sur les cp retenues
resuacp$ind$coord #composantes principales
tabcos2=resuacp$ind$cos2 #le tableau des cos2 des individus


#individus bien représentés sur C1 (cos2>0.5)
tabcos2[tabcos2[,1]>0.5,1]
#Estonie  Lettonie  Lituanie  Pays-Bas  Autriche   Pologne  Slovaqui     Suede 
#0.6352067 0.7284809 0.6849868 0.6432816 0.6874003 0.7263514 0.7726960 0.6680321 

#individus bien représentés sur C2 (cos2>0.25)
tabcos2[tabcos2[,2]>0.25,2]
#Danemark  Allemagn   Espagne    France   Irlande    Italie  Portugal 
#0.4071777 0.6794906 0.7146678 0.7602916 0.2557235 0.9071638 0.4512612


#individus bien représentés sur C3 (cos2>0.15)
tabcos2[tabcos2[,3]>0.15,3]
#Belgique  Rep-tche  Danemark  Allemagn     Grece   Irlande  Lituanie   Hongrie     Malte  Slovenie 
#0.2248240 0.3620024 0.1590900 0.1502900 0.5424276 0.2455376 0.1825275 0.3079377 0.4270459 0.4032351 
#Royaume- 
 # 0.2614944 

#individus bien représentés sur C4 (cos2>0.10)
tabcos2[tabcos2[,4]>0.10,4]
#Grece    Chypre     Malte 
#0.1154031 0.6008295 0.2006048 


# ------ Etape 5 : commentaire du graphique des pays

#Sur C1 : Suède, pays-bas et Autriche ont un EVH, EVH, PIBH
#et TACT élevés et TCHOM et TCHOMLD faibles
# Et pour la Pologne, la Slovaquie, La lettonie, Lituanie et l'Estonie
#c'est le contraire
#Sur C2 : France, All, Espagne, Italie :  POP et TEL élevés
#Danemark, Irlande Portugal : POP et TEL faibles
#Sur C3 : rep Tchèque, danemark, Allemagne, Lituanie, Royaume-Uni,
#ont une fort TEMP et un TINF faible
#Belgique, Grèce, Irlande, Hongrie, malte Slovénie : faible TEMP
#et fort TINF
#Sur C4 : Taux de mariage TRES élevé pour CHYPRE et élevé pour Malte
#et Grèce


#######################################################################
#######################################################################
#Avec Factoshiny
library(Factoshiny)
PCAshiny(pays2)
help(PCAshiny)



#Vérification : slide contribution d'une variable à un axe : 
#pour l'axe 1 : 
sum(resuacp$var$cor[,1]^2)  
resuacp$eig[1,1]
  #les deux sont égales 

#contribution axe 1 : 
resuacp$var$contrib[,1]
resuacp$var$cor[,1]^2 * 100 / resuacp$eig[1,1] 
  #les deux sont égales 





 
