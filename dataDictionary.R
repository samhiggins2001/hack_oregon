

colSummaries<-function(ftab){
	
	sumtab = matrix(data="",nrow=ncol(ftab), ncol=3)
	
	for(i in 1:ncol(ftab)){
		cn = names(ftab)[i]
		cat("\n\nCurrent column:", cn, "\n")
		cur = ftab[[cn]]
		
		stat = paste("Number of unique values in column:", 
								 length(unique(cur)), 
								 ";",
								 isBlank(cur),
								 "values are blank\n")
		cat(stat)
		cat("Here is a sample of the unique values found in this column:\n")
		print(head(unique(cur)))
		ctab = table(cur)[order(table(cur), decreasing=T)]
		pctab = paste(ctab, names(ctab),  sep=" record(s) is/are equal to ")
		top10 = paste("These are the numbers of times the 10 most common values occured:",paste(head(pctab, 10), collapse=" ; ", sep=";"))
		
		cat(top10,"\n")
		print(head(pctab, 10))
		sumtab[i,] = c(cn,stat,top10)
		
	}
	return(sumtab)
}
