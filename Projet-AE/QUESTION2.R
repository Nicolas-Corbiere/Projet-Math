#QUESTION 2
d1=read.table("../BDD/student-mat.csv",sep=",",header=TRUE)
d2=read.table("../BDD/student-por.csv",sep=",",header=TRUE)
mydata=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","guardian","traveltime","studytime","health","Walc","Dalc","goout","freetime","famrel","romantic","higher","activities","famsup","schoolsup"))

View(mydata)
# Est-ce que c’est vrai que le taux de reussite est supérieur chez les étudiants qui n’abusent pas d’alcool ?

noAlcool <- subset(mydata, Walc+Dalc<6)
Alcool <- subset(mydata, Walc+Dalc>5)

sumNoteNoAlcool <- (sum(noAlcool$G1.x) + sum(noAlcool$G2.x) + sum(noAlcool$G3.x) + sum(noAlcool$G1.y) + sum(noAlcool$G2.y) + sum(noAlcool$G3.y))/6
TotalNoAlcool <- nrow(noAlcool)
MoyenneNoAlcool <- sumNoteNoAlcool/TotalNoAlcool
print(MoyenneNoAlcool) # 11.80

sumNoteAlcool <- (sum(Alcool$G1.x) + sum(Alcool$G2.x) + sum(Alcool$G3.x) + sum(Alcool$G1.y) + sum(Alcool$G2.y) + sum(Alcool$G3.y))/6
TotalAlcool <- nrow(Alcool)
MoyenneAlcool <- sumNoteAlcool/TotalAlcool
print(MoyenneAlcool) # 10.29


#moyenne des personnes qui abusent de l'alcool < moyenne des personnes qui n'abusent pas de l'alcool
#10.29 < 11.79

# Donc il est vrai que le taux de reussite est supérieur chez les étudiants qui n’abusent pas d’alcool !


# 2.2 - Échantillonage

echanNoAlcool <- noAlcool[sample(1:nrow(noAlcool), 30,replace=FALSE), ]
echanAlcool <- Alcool[sample(1:nrow(Alcool), 30, replace=FALSE), ]


# 2.3 - Test d'Hypothèse 
# Nous alons prendre en considération que alpha = 5%

# 2.3.1 Consommation faible

# Nous supposons que les étudiants qui boivent peu d'alcool ont des meilleures notes à l'école
# et donc nous pensons que leur moyenne générale global sera de H0 = 12

testNA <- t.test((echanNoAlcool$G1.x + echanNoAlcool$G2.x + echanNoAlcool$G3.x + echanNoAlcool$G1.y + echanNoAlcool$G2.y + echanNoAlcool$G3.y)/6, mu=12)
testNA$p.value # Affichage de la p-value


# Nous avons une p-value égal a 0.23, cette valeur est supérieur 
# a alpha (0.05). De ce fait, nous ne rejetons pas notre hypothèse H0.

# 2.3.2 Consommation élevé

# Nous supposons que les étudiants qui boivent beaucoup d'alcool ont des moins bonnes notes à l'école
# et donc nous pensons que leur moyenne générale global sera de H0 = 10

testA <- t.test((echanAlcool$G1.x + echanAlcool$G2.x + echanAlcool$G3.x + echanAlcool$G1.y + echanAlcool$G2.y + echanAlcool$G3.y)/6, mu=10)
testA$p.value # Affichage de la p-value

# Nous avons une p-value égal a 0.53, cette valeur est supérieur 
# a alpha (0.05). De ce fait, nous ne rejetons pas notre hypothèse H1.

# 2.4 Graphs
library(help="graphics")

x = c(MoyenneNoAlcool, MoyenneAlcool)
noms_barres <- c("faible", "élevé")
barplot(x,ylim=c(0, 20), xlab="Degrés de consommation d'alcool",ylab="Résultat scolaire", ,names.arg=noms_barres);   box(title(main = "Impact de la consommation d'alcool sur le taux de réussite des étudiants"))
