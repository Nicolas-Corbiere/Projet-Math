#QUESTION 3
d1=read.table("../BDD/student-mat.csv",sep=",",header=TRUE)
d2=read.table("../BDD/student-por.csv",sep=",",header=TRUE)
mydata=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","guardian","traveltime","studytime","health","Walc","Dalc","goout","freetime","famrel","romantic","higher","activities","famsup","schoolsup"))

View(mydata)

# est-ce que la proportion de garçons qui abusent d’alcool est significativement superieure a celle des filles ?
  

garçons <- subset(mydata, sex=="M")
filles <- subset(mydata, sex=="F")

garçonsAlcool <- subset(mydata, sex=="M" & Walc+Dalc<6)
fillesAlcool <- subset(mydata, sex=="F" & Walc+Dalc<6)

percentGarçonsAlcool <- (100*nrow(garçonsAlcool))/nrow(garçons)
percentFillesAlcool <- (100*nrow(fillesAlcool))/nrow(filles)

print(percentGarçonsAlcool) #69,14
print(percentFillesAlcool) #91.79

#Non !  La proportion de garçons qui abusent d’alcool est significativement inférieur à celle des filles



# 2.2 - Échantillonage

uM <- percentGarçonsAlcool
oM <- sd(garçonsAlcool$Dalc + garçonsAlcool$Walc)
tabGarçonsAlcool <- garçonsAlcool[sample(1:nrow(garçonsAlcool), 30,replace=FALSE), ]
echanGarçonsAlcool <- tabGarçonsAlcool$Dalc + tabGarçonsAlcool$Walc
print(echanGarçonsAlcool)


uM <- percentFillesAlcool
oM <- sd(fillesAlcool$Dalc + fillesAlcool$Walc)
tabFillesAlcool <- fillesAlcool[sample(1:nrow(fillesAlcool), 30,replace=FALSE), ]
echanFillesAlcool <- tabFillesAlcool$Dalc + tabFillesAlcool$Walc
print(echanFillesAlcool)

# 2.3 - Test d'Hypothèse 
# Nous alons prendre en considération que alpha = 5%

# 2.3.1 Consommation faible

# Nous supposons que ce sont les garçons qui abusent beaucoup de l'alcool
# et donc nous pensons que le pourcentage de garçons qui abusent de l'alcool sera de H0 = 80

tM <- (80 - percentGarçonsAlcool) / (oM/sqrt(30)) 
print(tM)


# Nous avons un résultat égal a 54.449 , cette valeur est supérieur 
# a alpha (0.05). De ce fait, nous ne rejetons pas notre hypothèse H0.

# 2.3.2 Consommation élevé

# Nous supposons que ce sont les filles qui abusent le moins de l'alcool
# et donc nous pensons que le pourcentage de filles qui abusent de l'alcool sera de H0 = 60

tM <- (60 - percentFillesAlcool) / (oM/sqrt(30)) 
print(tM)

# Nous avons un résultat égal a -159.451 , cette valeur est inférieur 
# a alpha (0.05). De ce fait, nous rejetons notre hypothèse H0.


# 2.4 Graphs
library(help="graphics")

x = c(percentGarçonsAlcool, percentFillesAlcool)
noms_barres <- c("garçons", "filles")
barplot(x,ylim=c(0, 100), xlab="Sexe",ylab="Proportion en %", ,names.arg=noms_barres);   box(title(main = "Proportion de garçons/filles qui abusent de l'alcool"))

  