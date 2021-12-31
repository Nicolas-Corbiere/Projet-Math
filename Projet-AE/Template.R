d1=read.table("../BDD/student-mat.csv",sep=",",header=TRUE)
View(d1)
d2=read.table("../BDD/student-por.csv",sep=",",header=TRUE)
View(d2)
mydata=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","guardian","traveltime","studytime","health","Walc","Dalc","goout","freetime","famrel","romantic","higher","activities","famsup","schoolsup"))

View(mydata)

print(mydata$schoolsup.x)
print(mydata$schoolsup.y)
