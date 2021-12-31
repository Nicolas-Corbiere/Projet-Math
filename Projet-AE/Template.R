d1=read.table("../BDD/student-mat.csv",sep=",",header=TRUE)
View(d1)
d2=read.table("../BDD/student-por.csv",sep=",",header=TRUE)
View(d2)
mydata=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

View(mydata)

