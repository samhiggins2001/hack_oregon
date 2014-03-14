#sifTest1.R
if(!require("date")){
	install.packages("date")
	library("date")
}

getDateRange<-function(tab, from, to, dateColName="Tran.Date"){
	cat("min val:", as.date(from), "max val:", as.date(to))
	retval = (as.date(from)<tab$Tran.Date) & (as.date(to)>tab$Tran.Date)
	return(retval)
}

#first thing, get all the contributions from the last election cycle: 2010-2012
convertToDateCols<-function(ftab){
	#start by converting all date columns to R dates
	dateCols = grep(pattern="date", x=colnames(ftab), ignore.case=T)
	
	for(ci in dateCols){
		print(colnames(ftab)[ci])
		ftab[,ci] = as.date(ftab[,ci])
		print(is.date(ftab[1,ci]))
	}
	return(ftab)
}

ftab = convertToDateCols(ftab)

head(ftab[dexi1,])


dexi1 = getDateRange(tab=ftab, from="1/1/2010", to="1/1/2013")
sum(dexi1)
# [1] 282l239

dexi2 = getDateRange(tab=ftab, from="11/15/2008", to="11/15/2010")
sum(dexi2)
#[1] 150552
dexi3 = getDateRange(tab=ftab, from="11/15/2006", to="11/15/2008")
sum(dexi3)

#first get the inner web : those who give and recieve:
#pull out those from the 2006 to 2008 election cycle:
ex3 = ftab[dexi3,]
midmen3 = getMiddleMen(ftab=ex3)
#pull out the twoways
sif3 = getFromToAmountTable(ftab=ftab[dexi3,])
sif3inter = sif3[!isBlank(col=sif3$from, retlv=T),]#sif3inter has the twoways
#now, how many of these transactions are unique?
dim(unique(sif3inter[,1:2]))
# > dim(unique(sif3inter[,1:2]))
# [1] 10357     2
# > dim(sif3inter)
# [1] 16732     3

repeatedExchanges =dim(sif3inter)[1]- dim(unique(sif3inter[,1:2]))[1]
cat("there were",repeatedExchanges, "exchanges that were not the first exchanges between two individuals in this election cycle")
sti = sif3inter
dupi = duplicated(x=sif3inter[,1:2])
duprecs = sif3inter[dupi,]
uinter = unique(sif3inter[,1:2])

agsti <- aggregate(sif3inter$amount, by=list(sif3inter$from,sif3inter$to),
										FUN=sum, na.rm=TRUE)
colnames(agsti)<-colnames(sti)
#remove negatives
agsti = agsti[agsti$amount>0,]

#save simple interaction format
sifout = cbind(agsti[,1], rep("right", time=nrow(agsti)), agsti[,2])

write.table(x=sifout, file="./testSIFout1.sif", quote=F, sep="\t", col.names=F, row.names=F)

#save edge attributes
eat = cbind(paste(agsti[,1], rep("(right)", time=nrow(agsti)),agsti[,2], sep=" "), rep("=", time=nrow(agsti)), agsti[,3])

write.table(x=eat, file="./testSIFout1EdgeAttibutes.sif", quote=F, sep="\t", col.names=F, row.names=F)

#clean ftab
ftab$Contributor.Payee.Committee.ID[conIdBlanki] = ""

#save node id - node name 
conIdBlanki  = isBlank(ftab$Contributor.Payee.Committee.ID, retlv=T)

#get all the contributors, match to IDs
nameEx1 = ftab[!conIdBlanki,c("Contributor.Payee","Contributor.Payee.Committee.ID")]
nameEx2 = unique(nameEx1)

nameEx11 = ftab[,c("Filer","Filer.Id")]
colnames(nameEx2)<-colnames(nameEx11)
nameEx22 = rbind(nameEx2, nameEx11)
nameEx22 = unique(nameEx22)
print(dim(nameEx22))
nameEx23 = nameEx22[!duplicated(nameEx22$Filer.Id),]
print(dim(nameEx23))
print(head(nameEx23))
colnames(nameEx23)<-c("filer", "id")

nameDict = nameEx23$filer
names(nameDict)<-nameEx23$id

#test for total number of unique entities in sifout
allEnt = c(sifout[,1], sifout[,3])
allEnt = unique(allEnt)
head(allEnt)
length(allEnt)

sum(!allEnt%in%names(nameDict))

#now, make a lookup table for the SIF data
head(sifout)

sifNodeTab = cbind(allEnt, nameDict[allEnt])
colnames(sifNodeTab)<-c("id","nombre")

write.table(x=sifNodeTab, quote=F,
						file="./nodeNamesAttributeFile.txt", sep="\t", 
						row.names=F, col.names=T)


#now, get aggregate money donated for each ID: 
#first pull all the rows from ftab with contributor ids
# get amounts from ex3, this contains just the contributions from our target cycle
contTab = ex3[!conIdBlanki,c("Contributor.Payee.Committee.ID", "Amount")]

agsti <- aggregate(contTab$Amount, 
									 by=list(contTab$Contributor.Payee.Committee.ID),
									 FUN=sum, 
									 na.rm=TRUE)

colnames(agsti)<-c("id","totalSent")


#now, get the subset of recipients who have also given:

ex4 = ex3[ex3$Filer.Id%in%ex3$Contributor.Payee.Committee.ID,]
ex4$Filer.Id
#now sum how much each of those recipients have recieved
ex5 = ex3[,c("Filer.Id","Amount")]
agsti2 <- aggregate(ex5$Amount, 
									 by=list(ex5$Filer.Id),
									 FUN=sum, 
									 na.rm=TRUE)
agsti2Dict = agsti2$x
names(agsti2Dict)<-agsti2$Group.1
colnames(agsti2)<-c("id","totalRecieved")

agsti3 = cbind(agsti, agsti2Dict[agsti$id])
colnames(agsti3)<-c("id", "totalSent", "totalRecieved")

mergedNodeAttributes = merge(x=sifNodeTab, y=agsti3, by.x="id", by.y="id")

mergedNodeAttributes$totalRecieved[is.na(mergedNodeAttributes$totalRecieved)] = 0

write.table(x=mergedNodeAttributes, file="./nodeAttributesDataTable.txt", sep="\t", row.names=F, col.names=T, quote=F)

