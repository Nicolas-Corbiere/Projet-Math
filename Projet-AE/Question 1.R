# Question 1

# Template d'extraction de Data

d1=read.table("../BDD/student-mat.csv",sep=",",header=TRUE)
d2=read.table("../BDD/student-por.csv",sep=",",header=TRUE)
mydata=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","guardian","traveltime","studytime","health","Walc","Dalc","goout","freetime","famrel","romantic","higher","activities","famsup","schoolsup"))

View(mydata)


# 1.1 - Calcul de la moyenne

# 1.1.1 Modeste

# Pour les élèves de milieu modeste 
mydataModeste <- subset(mydata,Medu == 2 & Fedu == 2 )
sumAlcoolM <- sum(mydataModeste$Dalc) + sum(mydataModeste$Walc)
TotalM <- nrow(mydataModeste)
moyenneM <- sumAlcoolM/TotalM
print(moyenneM) # 3.673469

# 1.1.2 Autre

# Pour les élèves d'autre milieu ou de milieu mixte 
mydataOther <- subset(mydata,Medu != 2 | Fedu != 2)
sumAlcoolO <- sum(mydataOther$Dalc) + sum(mydataOther$Walc)
TotalO <- nrow(mydataOther)
moyenneO <- sumAlcoolO/TotalO
print(moyenneO) # 3.794393

#Les élève dont les parents on un niveau d’instruction modeste
#ont un niveau d'alcool moins élever que les autre élève

# 1.2 - Échantillonage

uM <- moyenneM
oM <- sd(mydataModeste$Dalc + mydataModeste$Walc)
tabM <- mydataModeste[sample(1:nrow(mydataModeste), 30, replace=FALSE), ]
echanM <- tabM$Dalc + tabM$Walc
print(echanM)

uO <- moyenneO
oO <- sd(mydataOther$Dalc + mydataOther$Walc)
tabO <- mydataOther[sample(1:nrow(mydataOther), 75, replace=FALSE), ]
echanO <- tabO$Dalc + tabO$Walc
print(echanO)

# 1.3 - Test d'Hypothèse 
# Nous alons prendre en considération que alpha = 5%

# 1.3.1 Modeste

# Nous supposons que les élèves de parents modeste boivent beaucoup
# et donc nous pensons que la moyenne d'indicateur de taux
# d'alcoolémie est de H0 = 4.

tM <- (4 - moyenneM) / (oM/sqrt(30)) 
print(tM)

# Nous avons un résultat égal a 0.822, cette valeur est supérieur 
# a alpha (0.05). De ce fait, nous ne rejetons pas notre hypothèse H0.


# 1.3.2 Autre

# Nous supposons que les élèves d'autres milieux boivent moins
# et donc nous pensons que la moyenne d'indicateur de taux
# d'alcoolémie est de H0 = 3.

tM <- (3 - moyenneM) / (oM/sqrt(30)) 
print(tM)

# Nous avons un résultat égal a -1.70, cette valeur est inférieur 
# a alpha (0.05). De ce fait, nous rejetons notre hypothèse H0.

# 1.4 Graphs
library(help="graphics")

x = c(moyenneM,moyenneO)
noms_barres <- c("Modeste", "Autre")
barplot(x,ylim=c(0, 5), xlab="Niveau instruction parents",ylab="Consommation jouralière d'alcool", ,names.arg=noms_barres);   box(title(main = "Consommation moyenne d'alcool chez les étudiants en fonction du niveau d'instruction de leur parents"))
