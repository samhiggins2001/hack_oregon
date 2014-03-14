#treeMapFunctions
source('./EDAfunctions.R')


oneLevelTreeMap<-function(){
	
	#first query the db to get counts for book_types
	q1 = "select \"Book_Type\", sum(\"Amount\"), count(distinct \"Tran_Id\") as \"count\" from fins group by \"Book_Type\""
	dbres = dbiRead(query=q1, dbname="contributions")
	colnames(dbres)<-c("Book_Type","Amount","count")
	
	for(dbrow in 1:nrow(dbres)){
		dbres$Book_Type[dbrow] = paste(dbres[dbrow,1],"$",
																	 (dbres[dbrow,2]/1000000),"million in contributions; ",
																	 dbres[dbrow,3],"contributors", collapse=" ")
	}
	
	toChildChildJSON(ag1=dbres, tabname="Amount By Book_Type", jsonFname="./treeplotAmountByBookType.JSON", showRemainingCategory=T)
	
	
}

twoLevelTreeMap<-function(){
	
	#first query the db to get counts for book_types
	q2 = "select \"Book_Type\",\"Contributor_Payee\", sum(\"Amount\"), count(distinct \"Tran_Id\") 
				from fins 
				group by \"Book_Type\",\"Contributor_Payee\""
	dbres = dbiRead(query=q2, dbname="contributions")
	colnames(dbres)<-c("Book_Type","Amount","count")
	
	# 	for(dbrow in 1:nrow(dbres)){
	# 		dbres$Book_Type[dbrow] = paste(dbres[dbrow,1],"$",
	# 																	 (dbres[dbrow,2]/1000000),"million in contributions; ",
	# 																	 dbres[dbrow,3],"contributors", collapse=" ")
	# 	}
	
	dbag = getTopAggregate(agdf=dbres, numberPer=10, colname="Book_Type", plotResults=F)
	
	toChildChildJSON(ag1=dbag, tabname="Amount By Book_Type", jsonFname="./treeplotAmountByBookType.JSON", showRemainingCategory=F)
	
	
}


# breaksBins<-function(dset,bpoints = c(1,10,100, 1000, 10000)){
# 	
# 	res = rep(0, times=length(dset))
# 	for(i in 1:length(bpoints)){
# 		res[dset>i] = 
# 	}
# 	
# }
#getTopAggregate()
#gets aggregations for the top "numberPer" entities
#in each of the "colname" categories
#agdf: contains original data
#				columns must be in the order: 
#								parent, child, amount; they will be named <colname> "Entity","Amount"
#								internally
getTopAggregate<-function(agdf,numberPer,colname="Book_Type", plotResults=F){
	
	#clean the input
	if(ncol(agdf)==4){
		colnames(agdf)<-c(colname, "Entity", "Amount", "count")
	}else{
		colnames(agdf)<-c(colname, "Entity", "Amount")
	}
	
	badRows = is.na(agdf[,colname])
	if(sum(badRows)){
		cat(sum(badRows), "rows were found to have NA values for the Book_Type; these will be removed")
		agdf = agdf[!badRows,]
	}
	
	rownames(agdf)<-1:nrow(agdf)
	uVal = unique(agdf[,colname])
	
	histSet = list()
	distSet = list()
	
	dfout = NULL #data.frame(matrix(nrow=0,ncol=ncol(agdf), dimnames=list(NULL,colnames(agdf))))
	

	for(i in 1:length(uVal)){
		
		cat("\nCurrent book type: \"", uVal[i],"\"\n")
		#pull all the rows out
		cursub = agdf[agdf[,colname] == uVal[i],]
		print(colnames(cursub))
		#save the hist for latter
		if(plotResults){
			histSet[[uVal[i]]] = hist(log(cursub$Amount, base=10), plot=F)
			distSet[[uVal[i]]] = log(cursub$Amount,base=10)
		}
		#figure out which are the 25 top rows
		cursub = cursub[order(cursub$Amount, decreasing=T),]
		top25 = cursub[1:min(numberPer,nrow(cursub)),]
		if(nrow(cursub)>numberPer){#if there are more than numberPer records of the current colname type
			#then aggregate all the rest into an entity of type "remaining"+number
			#get everything not in rownames(top25)
			remSet = cursub[!rownames(cursub)%in%rownames(top25),]
			if(ncol(agdf)==4){
				print("4")
				remRow = cbind.data.frame(Book_Type=uVal[i], 
																	Entity = paste("Remaining ",nrow(cursub)-numberPer," ",uVal[i],"(s)", sep=""),
																	Amount=sum(remSet$Amount), 
																	count=sum(remSet$count))
			}else{
				print("not 4")
				remRow = cbind.data.frame(Book_Type=uVal[i], 
																	Entity = paste("Remaining ",nrow(cursub)-numberPer," ",uVal[i],"(s)", sep=""),
																	Amount=sum(remSet$Amount))
			}
			print(head(dfout))
			print(head(top25))
			print(remRow)
			dfout = rbind(dfout, top25, remRow)
		}else{
			dfout = rbind(dfout, top25)
		}
		
	}
	if(plotResults){
		boxplot(x=distSet, 
						varwidth=T, 
						notch=T, 
						las=2, 
						horizontal=T, 
						xlab="log10(contribution amount)", 
						main="Distributions of contribution amounts per book type\n box height = number of different contributions")
	}
	
	return(dfout)
}



recipientsTreeMap<-function(){
	
	#first query the db to get counts for book_types
	q1 = "select \"Book_Type\", sum(\"Amount\"), count(distinct \"\") 
				from fins 
				where 
				group by \"Book_Type\""
	dbres = dbiRead(query=q1, dbname="contributions")
	colnames(dbres)<-c("Book_Type","Amount","count")
	
	for(dbrow in 1:nrow(dbres)){
		dbres$Book_Type[dbrow] = paste(dbres[dbrow,1],"$",
																	 (dbres[dbrow,2]/1000000),"million in contributions; ",
																	 dbres[dbrow,3],"contributors", collapse=" ")
	}
	
	toChildChildJSON(ag1=dbres, tabname="Amount By Book_Type", jsonFname="./treeplotAmountByBookType.JSON", showRemainingCategory=T)
	
	
}

removeRemainingCategory<-function(ag){
	
	remRow = grepl(pattern="^Remaining [0-9]*", x=ag$Entity)
	agout = ag[!remRow,]
	return(agout)
}

test.toChildChild<-function(){
	toChildChild(ag1=ag1, tabname=tabname, showRemainingCategory=F)
	
}

toChildChildJSON<-function(ag1, tabname, jsonFname="./RTreeMapjsonRes.json", showRemainingCategory=F){
	library("RJSONIO")
	
	if(!showRemainingCategory) ag1=removeRemainingCategory(ag=ag1)
	
	ag1$Book_Type[is.na(ag1$Book_Type)] = "No book type provided"
	
	#1st: get the unique super childs
	usc = unique(ag1$Book_Type)
	#2nd: make the super list
	sl = list()
	length(sl)<-2
	names(sl) = c("children","name")
	sl$name = tabname
	childlist = list()
	
	#for each super child, add the sub child list:
	for(j in 1:length(usc)){
		cbt = usc[j]
		cl = list()
		
		if(!is.null(ag1$Entity)){
			
			cbti = which(ag1$Book_Type == cbt)
			cvl = list()
			
			for(i in 1:length(cbti)){
				
				curCbti = cbti[i]
				if(is.null(ag1$count)){
					cvl[[i]] = list(amount=ag1[curCbti,"Amount"],name=ag1[curCbti,"Entity"])
				} else {
					cvl[[i]] = list(amount=ag1[curCbti,"Amount"],name=ag1[curCbti,"Entity"],count=ag1[curCbti,"count"])
				}

			}
			cl$children = cvl
			cl$name=cbt
		}else{
			cl$name=cbt
			cl$amount = ag1$Amount[ag1$Book_Type==cbt]
			if(!is.null(ag1$count)) cl$count=ag1$count[ag1$Book_Type==cbt]
		}
		childlist[[j]] = cl
	}
	
	sl$children = childlist
	jsonRes = toJSON(sl)
	write.table(x=jsonRes, file=jsonFname, sep="\t", row.names=F, col.names=F, quote=F)
}


test.getTopAggregate<-function(){
	cleanFins = fins
	colname="Book_Type"
	forR = aggregate(x=cleanFins$Amount, 
									 by=list(cleanFins[,colname],cleanFins$Contributor_Payee), 
									 FUN=sum)
	
	colnames(forR)<-c(colname,"Entity","Amount")
	numberPer=10
	topBookType1 = getTopAggregate(agdf=forR, numberPer=numberPer, colname=colname)
	
	
	ag2 = aggregate(x=cleanFins$Amount, by=list(cleanFins[,colname]), FUN=sum)
	
	
	
	write.table(x=topBookType1, 
							file="./aggregatedTopBooktypes.txt", 
							sep="\t", 
							col.names=T,
							row.names=F,
							quote=T)
	
	write.table(x=forR, 
							file="./aggregatedBooktypes.txt", 
							sep="\t", 
							col.names=T,
							row.names=F,
							quote=T)
	
	colname2="Purpose.Codes"
	numberPer=25
	forR2 = aggregate(x=cleanFins$Amount, by=list(cleanFins$Filer,cleanFins[,colname2]), FUN=sum)
	colnames(forR2)<-c(colname2,"Aggregate.Amount")
	
	topPurposeCodes1 = getTopAggregate(agdf=forR2, numberPer=numberPer, colname=colname2)
	
}



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
		# 		if()
		#find the top 25 values
		top25Rows = rownames(agdf[valrows,])[order(agdf$Aggregate.Amount[valrows], decreasing=T)][1:numberPer]
		
		cmin = range(agdf$Aggregate.Amount[valrows][order(agdf$Aggregate.Amount[valrows], decreasing=T)[1:25]])[1]
		
		
		
		
	}
	
}

getDataForTreeplot<-function(){
	fins = read.table(header=T, 
										comment.char="",
										stringsAsFactors=F,
										sep="\t",
										file="./orestar/fins/RecordsConvertedToTxt/joinedTables.tsv")
	
	cat("Data dimensions:")
	print(dim(fins))
	
	fins = CleanTableExBadRows(tab=fins)
	
	#make them numeric
	fins$Amount = as.numeric(fins$Amount)
	fins$Aggregate_Amount = as.numeric(fins$Aggregate_Amount)
	
	finsEx = fins[,c("Book_Type","Contributor_Payee","Amount")]
	colname="Book_Type"
	agFins = 	aggregate(x=finsEx$Amount, 
											by=list(finsEx[,colname],finsEx$Contributor_Payee), 
											FUN=sum)
	colnames(agFins)<-c(colname,"Entity","Amount")
	agForTree = getTopAggregate(agdf=agFins, colname="Book_Type", numberPer=10)
	return(agForTree)
}




queryDbForAggregates<-function(){
	query1 = "select \"Book_Type\", \"Contributor_Payee\", sum(\"Amount\")
	from fins 
	where \"Filer_Id\"
	in (select \"Committee_Id\" from \"CC\")
	group by \"Book_Type\", \"Contributor_Payee\""
	
	query2 = "select \"Book_Type\", \"Contributor_Payee\", sum(\"Amount\")
	from fins 
	where \"Filer_Id\"
	in (select \"Committee_Id\" from \"PAC\")
	group by \"Book_Type\", \"Contributor_Payee\""
	
	dbres1 = dbiRead(query=query1, dbname="contributions")
	dbres2 = dbiRead(query=query2, dbname="contributions")
	
	dbres1$Contributor_Payee = unifyEntities(col=dbres1$Contributor_Payee)
	dbres2$Contributor_Payee = unifyEntities(col=dbres2$Contributor_Payee)
	
	ag_1 = aggregate(x=dbres1$sum, by=list(dbres1$Book_Type, dbres1$Contributor_Payee), FUN=sum)
	ag_2 = aggregate(x=dbres2$sum, by=list(dbres2$Book_Type, dbres2$Contributor_Payee), FUN=sum)
	colnames(ag_1)<-c("Book_Type","Entity","Amount")
	colnames(ag_2)<-c("Book_Type","Entity","Amount")
	
	ag1 = getTopAggregate(agdf=ag_1, numberPer=10, colname="Book_Type", plotResults=F)
	ag2 = getTopAggregate(agdf=ag_2, numberPer=10, colname="Book_Type", plotResults=F)
	
	write.table(x=ag1, file="./CCaggregates.txt", sep="\t",row.names=F, col.names=T)
	write.table(x=ag2, file="./PACaggregates.txt", sep="\t",row.names=F, col.names=T)
	
	
	
}

toChildChild<-function(ag1, tabname){
	
	
	#1st: get the unique super childs
	usc = unique(ag1$Book_Type)
	#2nd: make the super list
	sl = list()
	length(sl)<-2
	names(sl) = c("children","name")
	sl$name = tabname
	childlist = list()
	#for each super child, add the sub child list:
	for(j in 1:length(usc)){
		cbt = usc[j]
		cl = list()
		cbti = which(ag1$Book_Type == cbt)
		cvl = list()
		for(i in 1:length(cbti)){
			curCbti = cbti[i]
			cvl[[i]] = list(amount=ag1[curCbti,"Amount"],name=ag1[curCbti,"Entity"])
		}
		cl$children = cvl
		cl$name=cbt
		childlist[[j]] = cl
	}
	
	sl$children = childlist
	jsonRes = toJSON(sl)
	write.table(x=jsonRes, file="./RTreeMapjsonRes.json", sep="\t", row.names=F, col.names=F, quote=F)
}

toChild0<-function(ag1, tabname){
	
	
	#1st: get the unique super childs
	usc = unique(ag1$Book_Type)
	#2nd: make the super list
	sl = list()
	length(sl)<-2
	names(sl) = c("children","name")
	sl$name = tabname
	childlist = list()
	#for each super child, add the sub child list:
	for(cbt in usc){
		cl = list()
		cbti = which(ag1$Book_Type == cbt)
		cvl = list()
		for(i in cbti){
			cvl = c(cvl,list(amount=ag1[i,"Amount"],name=ag1[i,"Entity"]))
		}
		cl$children = cvl
		cl$name=cbt
		childlist = c(childlist, cl)
	}
	
	sl$children = childlist
	
}