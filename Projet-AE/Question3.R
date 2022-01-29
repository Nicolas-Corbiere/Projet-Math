#QUESTION 3
d1=read.table("../BDD/student-mat.csv",sep=",",header=TRUE)
d2=read.table("../BDD/student-por.csv",sep=",",header=TRUE)
mydata=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","guardian","traveltime","studytime","health","Walc","Dalc","goout","freetime","famrel","romantic","higher","activities","famsup","schoolsup"))

View(mydata)

# est-ce que la proportion de garçons qui abusent d’alcool est significativement superieure a celle des filles ?
  

garçons <- subset(mydata, sex=="M")
filles <- subset(mydata, sex=="F")

garçonsAlcool <- subset(mydata, sex=="M" & Walc+Dalc>5)
fillesAlcool <- subset(mydata, sex=="F" & Walc+Dalc>5)

percentGarçonsAlcool <- (nrow(garçonsAlcool))/nrow(garçons)
percentFillesAlcool <- (nrow(fillesAlcool))/nrow(filles)

print(percentGarçonsAlcool) #30 %
print(percentFillesAlcool) #8 %


#Oui !  La proportion de garçons qui abusent d’alcool est significativement supérieur à celle des filles



# 3.2 - Échantillonage


echanGarçons<- garçons[sample(1:nrow(garçons), 30,replace=FALSE), ]
pourcentEGAlcool <- nrow(subset(echanGarçons, Walc+Dalc>5))/nrow(echanGarçons)
  
echanFilles <- filles[sample(1:nrow(filles), 30,replace=FALSE), ]
pourcentEFAlcool <- nrow(subset(echanFilles, Walc+Dalc>5))/nrow(echanFilles)

# 3.3 - Test d'Hypothèse 
# Nous alons prendre en considération que alpha = 5%

# 3.3.1 Garçons

# Nous supposons que ce sont les garçons qui abusent beaucoup de l'alcool
# et donc nous pensons que le pourcentage de garçons qui abusent de l'alcool sera de H0 = 25

h0 <- 0.25
ecartTypeH0 <- sqrt(h0 * (1 - h0))
ecartTypeHPG <- ecartTypeH0/sqrt(nrow(echanGarçons))
valCritG <- (pourcentEGAlcool - h0)/ecartTypeHPG

# Nous avons un résultat égal a 1.05. cette valeur est supérieur 
# a alpha (0,05). De ce fait, nous ne rejetons pas notre hypothèse H0.

# 2.3.2 Filles

# Nous supposons que ce sont les filles qui abusent le moins de l'alcool
# et donc nous pensons que le pourcentage de filles qui abusent de l'alcool sera de H0 = 10

h1 <- 0.10
ecartTypeH1 <- sqrt(h1 * (1 - h1))
ecartTypeHPF <- ecartTypeH1/sqrt(nrow(echanFilles))
valCritF <- (pourcentEFAlcool - h1)/ecartTypeHPF

# Nous avons un résultat égal a 0. Cette valeur est inférieur 
# a alpha (0.05). De ce fait, nous rejetons notre hypothèse H0.
# Ce résultat est dû au fait qu'il y a très peu de filles alcoolique.


# 3.4 Graphs
library(help="graphics")

x = c(percentGarçonsAlcool*100, percentFillesAlcool*100)
noms_barres <- c("garçons", "filles")
barplot(x,ylim=c(0, 100), xlab="Sexe",ylab="Proportion en %", ,names.arg=noms_barres);   box(title(main = "Proportion de garçons/filles qui abusent de l'alcool"))

  