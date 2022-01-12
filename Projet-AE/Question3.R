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

percentGarçonsAlcool <- (nrow(garçonsAlcool))/nrow(garçons)
percentFillesAlcool <- (nrow(fillesAlcool))/nrow(filles)

print(percentGarçonsAlcool) #69 %
print(percentFillesAlcool) #92 %

#Non !  La proportion de garçons qui abusent d’alcool est significativement inférieur à celle des filles



# 3.2 - Échantillonage

uG <- percentGarçonsAlcool/100
oG <- sd(garçonsAlcool$Dalc + garçonsAlcool$Walc)
tabGarçonsAlcool <- garçonsAlcool[sample(1:nrow(garçonsAlcool), 30,replace=FALSE), ]
echanGarçonsAlcool <- tabGarçonsAlcool$Dalc + tabGarçonsAlcool$Walc
print(echanGarçonsAlcool)


uF <- percentFillesAlcool/100
oF <- sd(fillesAlcool$Dalc + fillesAlcool$Walc)
tabFillesAlcool <- fillesAlcool[sample(1:nrow(fillesAlcool), 30,replace=FALSE), ]
echanFillesAlcool <- tabFillesAlcool$Dalc + tabFillesAlcool$Walc
print(echanFillesAlcool)

# 3.3 - Test d'Hypothèse 
# Nous alons prendre en considération que alpha = 5%

# 3.3.1 Garçons

# Nous supposons que ce sont les garçons qui abusent beaucoup de l'alcool
# et donc nous pensons que le pourcentage de garçons qui abusent de l'alcool sera de H0 = 80

tG <- (0.8 - uG) / (oG/sqrt(30)) 
print(tG)


# Nous avons un résultat égal a 0,544. cette valeur est supérieur 
# a alpha (0,05). De ce fait, nous ne rejetons pas notre hypothèse H0.

# 2.3.2 Filles

# Nous supposons que ce sont les filles qui abusent le moins de l'alcool
# et donc nous pensons que le pourcentage de filles qui abusent de l'alcool sera de H0 = 60

tF <- (0.6 - uF) / (oF/sqrt(30)) 
print(tF)

# Nous avons un résultat égal a -1,595. Cette valeur est inférieur 
# a alpha (0.05). De ce fait, nous rejetons notre hypothèse H0.


# 3.4 Graphs
library(help="graphics")

x = c(percentGarçonsAlcool*100, percentFillesAlcool*100)
noms_barres <- c("garçons", "filles")
barplot(x,ylim=c(0, 100), xlab="Sexe",ylab="Proportion en %", ,names.arg=noms_barres);   box(title(main = "Proportion de garçons/filles qui abusent de l'alcool"))

  