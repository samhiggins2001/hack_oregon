#hackOregon
if(!require("gdata")){
	install.packages("gdata")
	library("gdata")
}

if(!require("R.utils")){
	install.packages("R.utils")
	library("R.utils")
}


importAllXLSFiles<-function(indir="~/prog/hack_oregon/orestar/fins", 
														outsuffix=".txt"){
	errorDir=paste0(indir,"/problemSpreadsheets")
	curtab=NULL
	
	fulltab = NULL
	files = dir(indir)
	
	errorlog = c()
	errorFileNames = c()

	files = files[grepl(pattern=".xls$", files)]
	convertedFileNames = gsub(pattern=".xls",replacement=".txt",x=files)
	
	for(i in 1:length(files)){	
		
		srce = paste(indir, files[i], sep="/") 
		dest = paste(indir, convertedFileNames[i], sep="/")
		cat("\nchecking", files[i],"..")
		if(!file.exists(dest)){
			curtab = try(expr=read.xls(xls=srce, stringsAsFactors=F), silent=TRUE)
			if(is.null(nrow(curtab))){
				cat("..error while reading file\n")
				addToErrorLog(vals=c(paste("read.xls error:", curtab), 
														 files[i]))
				#move file to error folder
				file.rename(from=srce, to=paste(errorDir, basename(path=srce), sep="/"))
			}else{
				cat("..opened, attempting save..")
				write.table(x=curtab, 
										col.names=T, 
										row.names=F, 
										quote=F,
										file=dest,
										sep="\t")
				testread = try(read.table(file=dest,
														 strip.white=T,
														 quote="",
														 comment.char="",
														 check.names=F,
														 header=T, 
														 sep="\t", 
														 stringsAsFactors=F))
				if(is.null(nrow(testread))){
					cat("..error while reading file\n")
					addToErrorLog(vals=c(paste("re read error:", testread), 
															 files[i]))
					#move files to error folder
					file.rename(from=dest, to=paste(errorDir, basename(path=dest), sep="/"))
					file.rename(from=srce, to=paste(errorDir, basename(path=srce), sep="/"))
				}
			}
		}else{
			cat("the converted file already exists\n")
		}#	if(!file.exists(dest))
	}#for
		
	errorTab = cbind.data.frame(errorlog, errorFileNames)
		
	write.table(x=errorTab, 
							file=paste0(errorDir, "errorLogTable.txt" ), 
							col.names=F, 
							row.names=F, 
							sep="\t", 
							quote=T)
	
	return(fulltab)
}
	
addToErrorLog<-function(vals, errorLogFname){
	
	if(dirname(errorLogFname)!="."&!file.exists(dirname(errorLogFname))){
		dir.create(path=dirname(errorLogFname), showWarnings=F, recursive=T)
	}
	
	if(file.exists(errorLogFname)){
		elog = read.table(file=errorLogFname, 
											header=F, 
											sep="\t",
											stringsAsFactors=F, 
											comment.char="")
		elog = rbind.data.frame(elog, vals)
		elog=unique(elog)
	}else{
		elog = as.data.frame(matrix(vals, nrow=1, dimnames=list(NULL,NULL)))
	}
	
	write.table(x=elog,
							sep="\t", 
							col.names=F, 
							row.names=F,
							file=errorLogFname)
	
}


mergeTxtFiles<-function(folderName="/Users/samhiggins2001_worldperks/prog/hack_oregon/orestar/fins"){
	
	allFiles = dir(folderName)
	txtFiles = allFiles[grepl(pattern=".txt$", x=allFiles, ignore.case=F, perl=T)]
	txtFiles = txtFiles[txtFiles!="problemSpreadsheetserrorLogTable.txt"]
	txtfilesfp = paste0(folderName,"/",txtFiles)

	totalLines = countLinesAllFiles(folderName=folderName)
	totalLines = totalLines-length(txtfilesfp)

	
	#make output data frame
	testRead = read.table(file=txtfilesfp[1],
										 strip.white=T,
										 quote="",
										 comment.char="",
										 check.names=F,
										 header=T, 
										 sep="\t", 
										 stringsAsFactors=F)
	tabout = data.frame(matrix(data="", nrow=totalLines, ncol=ncol(testRead), dimnames=list(NULL, colnames(testRead))), stringsAsFactors=F)
	curline = 1
	for(i in 1:length(txtfilesfp)){
		cat(i,"..")
		#open the file
		tabin = read.table(file=txtfilesfp[i],
							 strip.white=T,
							 quote="",
							 comment.char="",
							 check.names=F,
							 header=T, 
							 sep="\t", 
							 stringsAsFactors=F)	
		#add contents of file to tabout
		if(nrow(tabin)) tabout[curline:(curline+nrow(tabin)-1),] = tabin
		#update curline
		curline = curline + nrow(tabin)
	}
	
	write.table(x=tabout, sep="\t",col.names=T, row.names=F, 
							file=paste0(folderName, "/joinedTables.tsv"))
	return(tabout)
}

countLinesAllFiles<-function(folderName){
	allFiles = dir(folderName)
	txtFiles = allFiles[grepl(pattern=".txt$", x=allFiles, ignore.case=F, perl=T)]
	txtFiles = txtFiles[txtFiles!="problemSpreadsheetserrorLogTable.txt"]
	txtfilesfp = paste0(folderName,"/",txtFiles)
	
	#find the size of the needed data frame
	totalLines = 0
	for(i in 1:length(txtfilesfp)){
		clen = countLines(txtfilesfp[i])
		totalLines = totalLines + clen
		cat("file", i, "length", clen, "....")
	}
	cat("\n")
	return(totalLines)
}


readFinData<-function(fname){
	tabin = read.table(file=fname,
										 strip.white=T,
										 comment.char="",
										 check.names=F,
										 header=T, 
										 sep="\t", 
										 stringsAsFactors=F)	
	return(tabin)
}


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
