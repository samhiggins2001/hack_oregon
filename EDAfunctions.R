#EDAfunctions

library("ggplot2")


hbarplot<-function(barDict, maxLevels=5, barPlotTitle="Counts across types", left_margin_factor=1){
	
	if(length(barDict)>maxLevels){
		barDict = barDict[order(barDict,decreasing=T)]
		barDict  = barDict[1:maxLevels]
	}
	
	oldmar <- par()$mar
	while(T){
		res = try({
			par(mar=c(5.1, max(4.1,max(left_margin_factor*nchar(names(barDict)))/2.5) ,4.1 ,2.1))
			#	try(displayGraph(w), silent=T)
			barplot(barDict, horiz=T, las=2, main = barPlotTitle, xlab="Number found in data set", names.arg=names(barDict))
			par(oldmar)
		}, silent=T)
		# 			if(!grepl(pattern="Error", x=res)) break
		if(is.null(res)) break
		par(oldmar)
		readline(prompt="There seems to have been an error with plotting the bar graph.\nPlease increase the size of the plot window, the press enter")
	}
	par(oldmar)
}

makeDonationSizeBarPlot <- function (fins) {
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
}

#getFromToAmountTable
getFromToAmountTable<-function(ftab, fromColName="Contributor.Payee.Committee.ID", toColName="Filer.Id", amountColName="Amount"){
	
	outTab = ftab[,c(fromColName, toColName, amountColName)]
	
	colnames(outTab)<-c("from", "to", "amount")
	return(outTab)
}


