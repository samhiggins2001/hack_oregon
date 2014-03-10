

comtab = read.table(file="./orestar/comms/joinedTables.tsv", sep="\t", header=T, stringsAsFactors=F)


unique(comtab$Committee.Type)


fintab = read.table(file="./orestar/fins/joinedTables.tsv", sep="\t", header=T, stringsAsFactors=F)

#subset fintab

donorIn = fintab$Contributor.Payee.Committee.ID%in%comtab$Committee.Id

donly = fintab[donorIn,]
nonCom= fintab[!donorIn,]

#for the non-com: aggregate by total amount

dset  = nonCom$Amount


btt = table(fintab$Book.Type)

bttC = aggregate(x=fintab$Amount, by=list(fintab$Book.Type), FUN=sum)

bttCv  = bttC$x
names(bttCv)<-bttC$Group.1

summarize_by(col=fintab$Book.Type, 
						 display=T,
						 barPlotTitle="The contents of the Book.Type column")


hbarplot(barDict=bttCv, 
				 maxLevels = 15,
				 barPlotTitle="Money from each group", 
				 left_margin_factor=1)

par(oldmar)
print(out)

plot(btt, y=bttCv)

pcodesCounts = table(fins$Purpose.Codes)

pcodesCounts = pcodesCounts[order(pcodesCounts, decreasing=T)]

psub  = fins[fins$Purpose.Codes%in%names(head(pcodesCounts, 10)),]

summarize_by(col=psub$Purpose.Codes, 
						 display=T, 
						 barPlotTitle="Counts of Purpose.Codes",
						 left_margin_factor=1.2)

pcA = aggregate(x=fins$Amount, by=list(fins$Purpose.Codes), FUN=sum)

pcaD = pcA$x
names(pcaD) = pcA$Group.1

pcaD = pcaD[names(pcaD)!=""]

hbarplot(barDict=pcaD, 
				 maxLevels = 15,
				 barPlotTitle="Money vs purpose", 
				 left_margin_factor=1)


cleanFins = fins[fins$Amount>=1,]

forR = aggregate(x=cleanFins$Amount, by=list(cleanFins$Book.Type,cleanFins$Contributor.Payee), FUN=sum)

colnames(forR)<-c("Book.Type","Entity","Aggregate.Amount")

uType = unique(forR$Book.Type)


boxplot(x=distSet, 
				width = ag2$x,
				notch=T, 
				las=2, 
				horizontal=T, 
				xlab="log10(contribution amount)", 
				main="Distributions of contribution amounts per book type\n box height = number of different contributions")



