# Question 1

# 1 - Calcul de la moyenne

# Pour les élèves de milieu modeste 
mydataModeste <- subset(mydata,Medu == 2 & Fedu == 2 )
sumAlcoolM <- sum(mydataModeste$Dalc) + sum(mydataModeste$Walc)
TotalM <- nrow(mydataModeste)
moyenneM <- sumAlcoolM/TotalM
print(moyenneM) # 3.673469

# Pour les élèves d'autre milieu ou de milieu mixte 
mydataOther <- subset(mydata,Medu != 2 | Fedu != 2)
sumAlcoolO <- sum(mydataOther$Dalc) + sum(mydataOther$Walc)
TotalO <- nrow(mydataOther)
moyenneO <- sumAlcoolO/TotalO
print(moyenneO) # 3.794393

#Les élève dont les parents on un niveau d’instruction modeste
#ont un niveau d'alcool moins élever que les qutre élève

# 2 - Échantillonage


