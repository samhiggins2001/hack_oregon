
if(!require("grDevices")){
	install.packages("grDevices")
	library("grDevices")
}



test.fixVals<-function(){
	
	col=years
	badval="0006"
	correctval="2006"
	
}

fixVals<-function(bad, correct, col, tab){
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

cleanData<-function(fin){
	fin = fin[!is.na(fin$Amount),]
	
	years = fixVals(bad="0007", correct="2007", col=years,tab=fin)
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
	
	
	return(list(out=outmat, marginals=)
}


fin=fins
outmat = breakByCont(fin=fin)



# > outmat
# 2013     2012     2011     2010      2009     2008       2007       2006   2001 1998     2004     2005  2002
# 10000 7737241.8 48019407 10060205 54641095 8377918.9 68293791 10516806.2 2050769.06    0.0    0 69480.82 18588.00     0
# 1000  8488698.4 47623363 12713263 40577263 9787408.9 36890155  8209368.2 1737209.74    0.0    0  7500.00 10587.10 16500
# 100   6153385.8 21135550  9019292 20825367 7138285.7 19563522  5788032.3  913619.30 1064.5    0   873.02  5293.00     0
# 0      965955.5  2100138  1335687  1833081  899221.3  1532017   721525.4   79955.72    7.0   25   112.00  1332.08     0
barplot(outmat, main="contributions", 
				ylab="Dollar amount",
				xlab="year", col=rainbow(7),
				legend = paste(rownames(outmat), "and above"))

