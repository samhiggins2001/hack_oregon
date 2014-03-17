
if(!require("grDevices")){
	install.packages("grDevices")
	library("grDevices")
}

library("data.table")



dataCleaningProcedure<-function(){
	
	tabname = "fins"
	q1 = paste0("SELECT * FROM ", tabname)
	
	
	restab1 = dbiRead(query=q1, dbname="contributions")
	
	#remove white space
	
	restab1$Contributor_Payee = unifyEntities(col=restab1$Contributor_Payee)
	
	
	dbiWrite(tabla=restab1, name="fins", append=F, dbname="contributions")
	
}


makeUniqueContributorColumn<-function(){
	
	genericNames = c("Miscellaneous Cash Contributions $100 and under ",
	"Miscellaneous Cash Expenditures $100 and under",
	"Miscellaneous Personal Expenditures $100 and under",
	"",
	"Miscellaneous In-Kind Contributions $100 and under")
	
}


test.fixVals<-function(){
	
	col="years"
	bad="0006"
	correct="2006"
	
}

fixVals<-function(bad, correct, col, tab){
	maptabFname = "./typoMappingTable.txt"
	maptab = NULL
	
	newrow = c(col, bad, correct)
	
	if(file.exists(maptabFname)){
		maptab = read.table(file=maptabFname, header=T, sep="\t", stringsAsFactors=F, comment.char="")
		maptab = rbind.data.frame(maptab, newrow)
		maptab = unique(maptab)
	}else{
		maptab = data.frame(matrix(data = newrow, nrow=1, ncol=3, dimnames=list(NULL, c("columnName", "typo", "correction"))))
	}
	write.table(x=maptab, file=maptabFname, append=F, row.names=F, col.names=T, sep="\t")
	bi = col==bad
	print(tab[bi,])
	
	if(correct==""){
		uin = readline("Please enter the correction:")
		
		col[bi]=uin
		cat("\nFixed", sum(bi), "values.\n")
	}else{
		
		uin = readline("Press enter to accept change; enter n to refuse the change")
		
		if(uin==""){
			
			col[bi]=correct
			cat("\nFixed", sum(bi), "values.\n")
		}
		
	}
	
	return(col)
	
}

# col=dbres1$Contributor_Payee
unifyEntities<-function(col){
	
	#remove punctuation
	col = gsub(pattern="[().!~,*`/\\]",replacement=" ", x=col)
	col = gsub(pattern="[-_]",replacement=" ", x=col)
	#remove variants, like Inc.
	col = gsub(pattern="Inc", replacement="", x=col)
	#normalize 'and'
	col = gsub(pattern="( and )|[+]", replacement="&", x=col, ignore.case=T)
	col = gsub(pattern="&", replacement=" & ", x=col, ignore.case=T)
	#remove white space
	col = gsub(pattern="[ ]+", replacement=" ", x=col)
	col = gsub(pattern="^ ", replacement="", x=col)
	col = gsub(pattern=" $", replacement="", x=col)
	col = gsub(pattern="\t", replacement=" ", x=col)
	#make all the same case
	col = toupper(x=col)
	return(col)
}


cleanData<-function(fin){
	fin = fin[!is.na(fin$Amount),]
	
	years = fixVals(bad="0007", correct="2007", col=years, tab=fin)
	years = fixVals(bad="0008",correct="2008",col=years,tab=fin)
	years= fixVals("0009","2009",years,tab=fin)
	years= fixVals("0029","2009",years,tab=fin)
	years= fixVals("0108","2008",years, tab=fin)
	years = fixVals("0029","2009",years, tab=fin)
	table(years)
	years = fixVals("0200","",years, tab=fin)
	table(years)
	years = fixVals("0207","2007",years, tab=fin)
	table(years)
	years = fixVals("","",years, tab=fin)
	table(years)
	
	daymonth = gsub(pattern="[/][0-9]+$",replacement="",x=fin$Tran.Date)
	fulldate = paste(daymonth, years, sep="/")
	fin$Tran.Date = fulldate
	
	
	write.table(x=fin, sep="\t",col.names=T, row.names=F, 
							file=paste0(folderName, "/joinedTables.tsv"))
	
}

#'@title breakByCont
#'@description Makes matrix describing total political contributions by year from contributions in the ranges provided by the breaks arg.
#'@param breaks: the break points of the contribution bins (provided in descending order)
#'@param fin: the table of political contributions (must have columns "Tran.Date" and "Amount" containing the contribution date and dollar amount, respectively)
#'@return matrix: rows = ranges of contribution amount (demarked as the upper bound of contribution amount); columns=the years
breakByCont<-function(fin, breaks=c(50000,10000, 5000, 1000, 500, 100)){
	
	fin = fin[!is.na(fin$Amount),]
	
	years = gsub(pattern="^[0-9]+[/][0-9]+[/]", replacement="", x=fin$Tran.Date)
	breaks = c(breaks, 0)
	uyears = unique(years)
	outmat = matrix(data=0, nrow=length(breaks), ncol=length(uyears), dimnames=list(breaks, uyears))
	marginals = rep()
	for(y in uyears){#for each year
		cyear = fin[years==y,]
		
		for(i in 1:length(breaks)){
			curi = cyear$Amount>breaks[i]
			print(sum(curi))
			
			overmin = cyear[curi,]
			total = sum(overmin$Amount)
			outmat[as.character(breaks[i]),y] = total
			cyear = cyear[!curi,]
		}
	}
	
	return(outmat)
	
}


CleanTableExBadRows<-function(tab){
	print("Fixing the headers")
	tab = fixHeaders(tab=tab)
	print("Unifying the representation of not available values")
	tab = unifyNAs(tab=tab)
	#first find any rows without a transaction id
	#second find all the rows where the amount cant be converted to numeric
	print("Assuring Amount and Aggregate_Amount columns are of numeric types")
	initialRows = nrow(tab)
	tab$Amount = as.numeric(tab$Amount)
	tab$Aggregate_Amount = as.numeric(tab$Aggregate_Amount)
	tab = tab[!is.na(tab$Tran_Id)&(tab$Tran_Id!="")&!is.na(tab$Amount)&!is.na(tab$Aggregate_Amount),]
	rowsLost =   initialRows - nrow(tab)
	cat(rowsLost, "rows where lost be cleaning out rows without\nvalid contribution amounts or transaction IDs")
	return(tab)
}

unifyNAs<-function(tab){
	
	tab[is.na(tab)] = NA
	tab[tab=="NA"] = NA
	tab[tab=="<NA>"] = NA
	tab[tab==""]  = NA
	tab[is.na(tab)] = ""
	return(tab)
	
}

fixHeaders<-function(tab){
	#strip leading ".X"
	colnames(tab)<-gsub(pattern="^X.", replacement="", x=colnames(tab))
	colnames(tab)<-gsub(pattern="[.]$", replacement="", x=colnames(tab))
	colnames(tab)<-gsub(pattern="[.]", replacement="_", x=colnames(tab))
	return(tab)
}



#isBlank
#takes: col: vector of values to be checked for ("" or NA)
#				retlv: T/F flag indictaing if a logical vector of blank indexes should be returned
#returns the number of blank  ("" or NA) values or logical index of NA values
isBlank<-function(col, retlv=F){
	li = (is.na(col))|(col=="")
	if(retlv) return(li)
	return(sum(li, na.rm=T))
}




getCommitteeZipCodes<-function(){
	
	query="select * from comms"
	
	dbres = dbiRead(query=query, dbname="contributions")
	
	canzips = sapply(X=dbres$Candidate_Maling_Address, FUN=extractZip)
	names(canzips)<-NULL
	
	tresZips1 = sapply(X=dbres$Treasurer_Mailing_Address, 
										 FUN=extractZip)
	names(tresZips1)<-NULL
	tresZips = tresZips1
	tresZips[!is.na(canzips)] = canzips[!is.na(canzips)]
	
	write.table(x=cbind.data.frame(candidate_id=dbres$Committee_Id, zip=tresZips), sep="\t",
							quote=F,row.names=F, col.names=T,
							file="./tresZips_candWhereAvailable.txt")
	write.table(x=cbind.data.frame(candidate_id=dbres$Committee_Id, zip=tresZips1), sep="\t",
							quote=F,row.names=F, col.names=T,
							file="./tresZips.txt")
}



test.extractZip<-function(){
	tadd = "15716 SE Millmain DR Portland OR 97233"
	checkEquals(target="97233", current=extractZip(addr=tadd))
	tadd = "15716 SE Millmain DR Portland OR    97233"
	checkEquals(target="97233", current=extractZip(addr=tadd))
	
	tadd = "15716 SE Millmain DR Portland OR \t 97233"
	checkEquals(target="97233", current=extractZip(addr=tadd))
}

extractZip<-function(addr){
	addr = gsub(pattern="[ ]+", replacement=" ", x=addr)
	addr = gsub(pattern="[\t]", replacement=" ", x=addr)
	sadd = strsplit(x=addr, split=" ")[[1]]
	zipout = sadd[[length(sadd)]]
	return(zipout)
	# 	gsub(pattern="^[0-9]+ [a-zA-Z ]+", replacement="", x=addr, perl=T)
}











