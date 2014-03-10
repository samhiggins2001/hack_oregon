#eda.R


fins = read.table(header=T, 
									comment.char="",
									stringsAsFactors=F,
					 				sep="\t",
					 				file="./orestar/fins/joinedTables.tsv")


cat("Data dimensions:")
print(dim(fins))

colnames(fins)
# > colnames(fins)
# [1] "Tran.Id"                        "Original.Id"                   
# [3] "Tran.Date"                      "Tran.Status"                   
# [5] "Filer"                          "Contributor.Payee"             
# [7] "Sub.Type"                       "Amount"                        
# [9] "Aggregate.Amount"               "Contributor.Payee.Committee.ID"
# [11] "Filer.Id"                       "Attest.By.Name"                
# [13] "Attest.Date"                    "Review.By.Name"                
# [15] "Review.Date"                    "Due.Date"                      
# [17] "Occptn.Ltr.Date"                "Pymt.Sched.Txt"                
# [19] "Purp.Desc"                      "Intrst.Rate"                   
# [21] "Check.Nbr"                      "Tran.Stsfd.Ind"                
# [23] "Filed.By.Name"                  "Filed.Date"                    
# [25] "Addr.book.Agent.Name"           "Book.Type"                     
# [27] "Title.Txt"                      "Occptn.Txt"                    
# [29] "Emp.Name"                       "Emp.City"                      
# [31] "Emp.State"                      "Employ.Ind"                    
# [33] "Self.Employ.Ind"                "Addr.Line1"                    
# [35] "Addr.Line2"                     "City"                          
# [37] "State"                          "Zip"                           
# [39] "Zip.Plus.Four"                  "County"                        
# [41] "Purpose.Codes"                  "Exp.Date" 
summary(object=fins)

tabsums = list()

for(c in colnames(fins)){
	cat("\n\nThis column:", c,"\n")
	tabsums[[c]] = as.data.frame(as.matrix(table(fins[[c]])))#table(fins[[c]])
	print(length(tabsums[[c]]))
	if(nrow(tabsums[[c]])<100) print( tabsums[[c]] )
}

hist(ftab$Amount[ftab$Amount<100000], 
		 breaks=100, 
		 main="Histogram of contribution amount",
		 ylab="number of contributions",
		 xlab="contribution amount")


hist(log(ftab$Amount, base=10), 
		 breaks=100, 
		 main="Histogram of contribution amount",
		 ylab="number of contributions",
		 xlab="log10(contribution amount)")

#data dictionary commands
dim(ftab)
colnames(ftab)
colnames(ftab)
length(unique(ftab$Tran.Id))
dim(ftab)
length(unique(ftab$Original.Id))
unique(ftab$Tran.Status)
table(ftab$Tran.Status)
contTab = table(ftab$Contributor.Payee)
length(contTab)
max(contTab)
contTab = order(contTab, decreasing=T)
head(contTab)
contTab
contTab = order(contTab, decreasing=T)
contTab = table(ftab$Contributor.Payee)
contTab = contTab[order(contTab, decreasing=T)]
head(contTab)
length(contTa)
length(contTab)
dim(ftab)
unique(ftab$Sub.Type)
table(ftab$Sub.Type)
table(ftab$Sub.Type)
table(ftab$Sub.Type)
table(ftab$Sub.Type)[order(table(ftab$Sub.Type))]
table(ftab$Sub.Type)[order(table(ftab$Sub.Type), decreasing=T)]
hist(ftab$Amount)
hist(ftab$Amount, breaks=1000)
hist(ftab$Amount, breaks=1000, main="Histogram of contribution amount",ylab="number of contributions",xlab="contribution amount")
hist(ftab$Amount, breaks=100, main="Histogram of contribution amount",ylab="number of contributions",xlab="contribution amount")
hist(ftab$Amount[ftab$Amount>100],
breaks=100,
main="Histogram of contribution amount",
ylab="number of contributions",
xlab="contribution amount")
max(ftab$Amount)
hist(ftab$Amount[ftab$Amount<1000000],
breaks=100,
main="Histogram of contribution amount",
ylab="number of contributions",
xlab="contribution amount")
hist(ftab$Amount[ftab$Amount<100000],
breaks=100,
main="Histogram of contribution amount",
ylab="number of contributions",
xlab="contribution amount")
min(ftab$Amount)
range(ftab$Amount)
range(ftab$Aggregate.Amount)
hist(log(ftab$Amount),
breaks=100,
main="Histogram of contribution amount",
ylab="number of contributions",
xlab="contribution amount")
hist(log(ftab$Amount, base=10),
breaks=100,
main="Histogram of contribution amount",
ylab="number of contributions",
xlab="contribution amount")
hist(log(ftab$Amount, base=10),
breaks=100,
main="Histogram of contribution amount",
ylab="number of contributions",
xlab="contribution amount")
hist(log(ftab$Amount, base=10),
breaks=100,
main="Histogram of contribution amount",
ylab="number of contributions",
xlab="log10(contribution amount)")
unique(ftab$Contributor.Payee.Committee.ID)
contab = table(ftab$Contributor.Payee.Committee.ID)
contab = contab[order(contab)]
head(contab)
contab
sum(ftab$Contributor.Payee.Committee.ID=="")
sum(is.na(ftab$Contributor.Payee.Committee.ID))
length(unique(ftab$Filer.Id))
length(unique(ftab$Filer))
length(unique(ftab$Attest.By.Name))
unique(ftab$Attest.By.Name)
unique(ftab$Review.By.Name)
sum(is.na(ftab$Review.By.Name))
sum(ftab$Review.By.Name=="")
sum(ftab$Review.By.Name=="nanfer")
sum(ftab$Review.By.Name=="nanfer", na.rm=T)
sum(ftab$Review.By.Name=="", na.rm=T)
length(unique(ftab$Review.Date))
length(unique(ftab$Attest.Date))
length(unique(ftab$Occptn.Ltr.Date))
sum(is.na(ftab$Occptn.Ltr.Date))
unique(ftab$Occptn.Ltr.Date)
unique(ftab$Pymt.Sched.Txt)
table(ftab$Pymt.Sched.Txt)
unique(ftab$Filer)
unique(ftab$Purp.Desc)
purptab = table(ftab$Purp.Desc)
sum(is.na(ftab$Purp.Desc))
length(purptab)
sum(ftab$Purp.Desc=="", na.rm=T)
purptab = purptab[order(purptab,decreasing=T)]
head(purptab)
length(unique(ftab$Intrst.Rate))
sum(ftab$Intrst.Rate=="", na.rm=T)
sum(is.na(ftab$Intrst.Rate))
sum(ftab$Pymt.Sched.Txt=="", na.rm=T)
irtab = table(ftab$Intrst.Rate)

irtab = irtab[order(names(irtab))]

extab = ftab[(ftab$Intrst.Rate!=""),]

sum(is.na(ftab$Intrst.Rate))
sum(ftab$Intrst.Rate=="", na.rm=T)
blanki = ftab$Intrst.Rate==""|is.na(ftab$Intrst.Rate)
noni = ftab$Intrst.Rate%in%c("0","0.00%","-0-", "0 %", "0.", "0%", "0% Interest", "$o.oo","Zero","zero percent", "%0","non","none","None","o","O")
blanki = blanki|noni

extab = ftab[!blanki,]
dim(extab)
# [1] 1046   42
extab[,c(1,8,9,20)]

as.numeric(extab[,20])


morei = extab[,20]>extab[,8]
equali = extab[,20]=extab[,8]

length(unique(ftab$Check.Nbr))




isBlank(ftab$Check.Nbr)


isBlank(col=ftab$Tran.Stsfd.Ind)

isBlank(col=ftab$Filed.By.Name)
length(unique(ftab$Filed.By.Name))
head(unique(ftab$Filed.By.Name))
head(unique(ftab$Filed.Date))
length(unique(ftab$Filed.Date))

ftab$Filed.Date




head(ftab[,c(3,24)], n=10)
head(ftab[,c(32,33)])


head(ftab[,c(grep(pattern="date", x=colnames(ftab), ignore.case=T))])




