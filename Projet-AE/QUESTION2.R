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


sumNoteAlcool <- (sum(Alcool$G1.x) + sum(Alcool$G2.x) + sum(Alcool$G3.x) + sum(Alcool$G1.y) + sum(Alcool$G2.y) + sum(Alcool$G3.y))/6
TotalAlcool <- nrow(Alcool)
MoyenneAlcool <- sumNoteAlcool/TotalAlcool

#moyenne des personnes qui abusent de l'alcool < moyenne des personnes qui n'abusent pas de l'alcool
#10.29 < 11.79

# Donc il est vrai que le taux de reussite est supérieur chez les étudiants qui n’abusent pas d’alcool !