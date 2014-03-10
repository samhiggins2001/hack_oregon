#networkAnalysis1

openFundingTable<-function(fname="./orestar/fins/joinedTables.tsv"){
	
	fins = read.table(header=T, comment.char="",stringsAsFactors=F,
										sep="\t",
										file=fname)
	cat("\nColumn names:\n")
	print(colnames(fins))
	cat("\nData dimensions:", dim(fins)[1], "rows", dim(fins)[2], "columns.\n")
	return(fins)
}

test.yearSubset<-function(){
	
	tin = openFundingTable()
	years = c("2011")
	
}


yearSubset<-function(years,tin){
	pats = paste0(years, "$")
	yrows = grepl(pattern=years, x=tin, ignore.case=T)
	
}