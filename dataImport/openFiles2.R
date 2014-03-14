#hackOregon
source('~/prog/hack_oregon/dataImport/finDataImport.R')


comtab = read.table(file="./orestar/comms/joinedTables.tsv", sep="\t", header=T, stringsAsFactors=F)


unique(comtab$Committee.Type)


fintab = read.table(file="./orestar/fins/joinedTables.tsv", sep="\t", header=T, stringsAsFactors=F)

#subset fintab

donorIn = fintab$Contributor.Payee.Committee.ID%in%comtab$Committee.Id

donly = fintab[donorIn,]
nonCom= fintab[!donorIn,]

#for the non-com: aggregate by total amount

dset  = nonCom$Amount


breaksBins<-function(dset,bpoints = c(1,10,100, 1000, 10000)){
	
	res = rep(0, times=length(dset))
	for(i in 1:length(bpoints)){
		res[dset>i] = 
	}
	
}

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



getTopAggregate1<-function(agdf,numberPer,colname="Book.Type"){
	
	rownames(agdf)<-1:nrow(agdf)
	uVal = unique(agdf[,colname])
	exRows = rep(F, times=nrow(agdf))
	histSet = list()
	
	
	
	for(i in 1:length(uVal)){
		cat("\nCurrent book type: \"", uVal[i],"\"\n")
		#pull all the rows out
		valrows = agdf[,colname] == uVal[i]
		#save the hist for latter
		histSet[[uVal[i]]] = hist(log(agdf$Aggregate.Amount[valrows], base=10), plot=F)
		#figure out which are the 25 top rows
		if()
			#find the top 25 values
		top25Rows = rownames(agdf[valrows,])[order(agdf$Aggregate.Amount[valrows], decreasing=T)][1:numberPer]
		
		cmin = range(agdf$Aggregate.Amount[valrows][order(agdf$Aggregate.Amount[valrows], decreasing=T)[1:25]])[1]
		
		
		
		
	}
	
}

boxplot(x=distSet, 
				width = ag2$x,
				notch=T, 
				las=2, 
				horizontal=T, 
				xlab="log10(contribution amount)", 
				main="Distributions of contribution amounts per book type\n box height = number of different contributions")


# 
# 
# hist(x=log(agdf$Aggregate.Amount[valrows]), axis=function(...){},
# 		 main=paste("Distribution of contribution amounts for all contributors of type \"",uVal[i],"\""))
# 
# ggplot(agdf$Aggregate.Amount[valrows], aes(x = agdf$Aggregate.Amount[valrows])) + geom_histogram() + scale_x_log()
# 
# mydata_hist = hist(agdf$Aggregate.Amount[valrows], plot=F)
# plot(mydata_hist$count, log="y", type='h', lwd=10, lend=2)
# 
# hist.data$counts = log(hist.data$counts, 2)
# plot(hist.data)
# 
# breaks=10
# mdat = agdf$Aggregate.Amount[valrows]
# buckets <- seq(from=range(mdat)[1],to=range(mdat)[2], length.out=breaks)
# log(buckets)
# mydata_hist <- hist(agdf$Aggregate.Amount[valrows], plot=FALSE)
# bp <- barplot(mydata_hist$count, log="y", col="white", names.arg=buckets)
# text(bp, mydata_hist$counts, labels=mydata_hist$counts, pos=1)




# 	
# fullname = paste(indir, files[fn], sep="/")
# fullOutName = paste
# cat("\nFile",fn,"of",length(files),fullname, "size:", as.character(as.numeric(file.info(fullname)$size)/1000000), "Mb")
# cat("..reading..")
# curtab = try(expr=read.xls(xls=fullname, stringsAsFactors=F), silent=TRUE)
# if(is.null(nrow(curtab))){
# 	errorlog=c(errorlog, paste(curtab, fullname))
# }else{
# 	cat("..appending..")
# 	if(nrow(curtab)){
# 		write.table(x=curtab, 
# 								quote=F,
# 								file=outfile,
# 								sep="\t", 
# 								col.names=!file.exists(outfile), 
# 								row.names=F, 
# 								append=T)
# 		read.table(file=outfile, sep="\t")
# 	}
# }
# 
# 
# 	
# 	
# 	
# 
# outfile="~/prog/hack_oregon/orestar/fins/joinedTable.txt"
# 
# allFins = importAllXLSFiles(indir="~/prog/hack_oregon/orestar/fins", outfile="~/prog/hack_oregon/orestar/fins/joinedTable.txt")
# 
# allFins[35843,]
# #subtract line 35802:
# badRows=c(35802,35804)
# 
# allFins[allFins$Tran.Id=="1438143",]
# 
# allFins2 = allFins[!1:nrow(allFins)%in%badRows,]
# 
# 
# write.table(x=allFins2,
# 						file=outfile, 
# 						append=F,
# 						quote=T, 
# 						sep="\t", 
# 						row.names=F, 
# 						col.names=T)
# 
# curtab = read.table(file=outfile,
# 										comment.char="",
# 										check.names=F,
# 										header=T, 
# 										sep="\t", 
# 										stringsAsFactors=F)



# copyQuery1 = ""
# copyQuery2 = 
# 
# dbCall(sql=, dbname=contributions)

# 
# # iris.xls is included in the gregmisc package for use as an example
# xlsfile <- file.path(path.package('gdata'),'xls','iris.xls')
# xlsfile
# 
# iris <- read.xls(xlsfile) # defaults to csv format
# iris <- read.xls(xlsfile,method="csv") # specify csv format
# iris <- read.xls(xlsfile,method="tab") # specify tab format
# 
# head(iris)  # look at the top few rows
# 
# 
# iris <- read.xls(xlsfile, perl="/usr/bin/perl")
