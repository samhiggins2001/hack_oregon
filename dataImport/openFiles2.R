#hackOregon
if(!require("gdata")){
	install.packages("gdata")
	library("gdata")
}

if(!require("R.utils")){
	install.packages("R.utils")
	library("R.utils")
}
library("dplyr")


read.finance.txt<-function(fname){
	return(read.table(file=fname,
										allowEscapes=T,
										 strip.white=T,
										 comment.char="",
										 check.names=F,
										 header=T, 
										 sep="\t", 
										 stringsAsFactors=F))
}

write.finance.txt<-function(dat,fname){
	write.table(x=dat,file=fname, 
							append=F, 
							quote=T, 
							sep="\t", 
							row.names=F, 
							col.names=T, 
							qmethod="escape")
}

importAllXLSFiles<-function(indir="~/prog/hack_oregon/orestar/fins", 
														outsuffix=".txt", destDir=NULL, forceImport=F){
	
	indir = gsub(pattern="[/]$", replacement="", x=indir)
	
	if(is.null(destDir)){
		destDir = paste0(indir,"/RecordsConvertedToTxt")
		dir.create(path=destDir, showWarnings=F, recursive=T)
	}
	
	destDir = gsub(pattern="[/]$", replacement="", x=destDir)
	
	
	errorDir=paste0(indir,"/problemSpreadsheets")
	dir.create(path=errorDir, showWarnings=F,recursive=T)
	
	curtab=NULL
	
	fulltab = NULL
	files = dir(indir)
	
	errorlog = c()
	errorFileNames = c()

	files = files[grepl(pattern=".xls$", files)]
	convertedFileNames = gsub(pattern=".xls",replacement=".txt",x=files)
	
	for(i in 1:length(files)){	
		
		srce = paste(indir, files[i], sep="/") 
		dest = paste(destDir, convertedFileNames[i], sep="/")
		cat("\nchecking", files[i],"..")
		if(!file.exists(dest)|forceImport){
			curtab = try(expr=read.xls(xls=srce, comment.char="",quote="",
																 stringsAsFactors=F, 
																 method="tab"), silent=TRUE)
			if(is.null(nrow(curtab))){
				cat("..error while reading file\n")
				addToErrorLog(errorLogFname=paste0(errorDir,"/","errorLog.txt"),
											vals=c(paste("read.xls error:", curtab), 
														 files[i]))
				#move file to error folder
				file.rename(from=srce, to=paste(errorDir, basename(path=srce), sep="/"))
			}else{
				cat("..opened, attempting save..")
				write.finance.txt(dat=curtab, fname=dest)
# 				write.table(x=curtab, 
# 										qmethod="escape",
# 										col.names=T, 
# 										row.names=F, 
# 										quote=T,
# 										file=dest,
# 										sep="\t")
# 				testread = try(   read.table(file=dest,
# 														 strip.white=T,
# 														 comment.char="",
# 														 check.names=F,
# 														 header=T, 
# 														 sep="\t", 
# 														 stringsAsFactors=F))
				testread = try(read.finance.txt(dest), silent=T)
				
				if(is.null(nrow(testread))){
					cat("..error while reading file\n")
					addToErrorLog(errorLogFname=paste0(errorDir,"/","errorLog.txt"),vals=c(paste("re read error:", testread), 
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
		elog = try(expr=read.table(file=errorLogFname, 
											header=F, 
											sep="\t",
											stringsAsFactors=F, 
											comment.char=""), silent=T)
		if(is.null(nrow(elog))){
			elog = as.data.frame(matrix(vals, nrow=1, dimnames=list(NULL,NULL)))
		}else{
			elog = rbind.data.frame(elog, vals)
			elog=unique(elog)
		}

	}else{
		elog = as.data.frame(matrix(vals, nrow=1, dimnames=list(NULL,NULL)))
		dir.create(path=basename(path=errorLogFname), recursive=T, showWarnings=F)
	}
	print(errorLogFname)
	write.table(x=elog,
							sep="\t", 
							col.names=F, 
							row.names=F,
							file=errorLogFname)
	
}

# folderName="/Users/samhiggins2001_worldperks/prog/hack_oregon/orestar/comms"

mergeTxtFiles<-function(folderName="/Users/samhiggins2001_worldperks/prog/hack_oregon/orestar/fins"){
	
	allFiles = dir(folderName)
	txtFiles = allFiles[grepl(pattern=".txt$", x=allFiles, ignore.case=F, perl=T)]
	txtFiles = txtFiles[txtFiles!="problemSpreadsheetserrorLogTable.txt"]
	txtfilesfp = paste0(folderName,"/",txtFiles)

	totalLines = countLinesAllFiles(folderName=folderName)
	totalLines = totalLines-length(txtfilesfp)

	
	#make output data frame
	testRead = read.finance.txt(txtfilesfp[1])

	tabout = data.frame(matrix(data="", nrow=totalLines, ncol=ncol(testRead), dimnames=list(NULL, colnames(testRead))), stringsAsFactors=F)
	curline = 1
	for(i in 1:length(txtfilesfp)){
		
		#open the file
		tabin = read.finance.txt(txtfilesfp[i])
		#add contents of file to tabout
		if(nrow(tabin)){
			tabout[curline:(curline+nrow(tabin)-1),] = tabin
		}else{
			cat("\nBlank table:",txtfilesfp[i],"\n")
		}
		cat(i, txtfilesfp[i],"rows:",nrow(tabin),"\n")
		curline = curline + nrow(tabin)
	}
	#now cut off the data frame so that no blank rows are included
	tabout = tabout[1:curline-1,]

	cat("Total dimensions of merged file:", dim(tabout)[1],"rows", dim(tabout)[2],"columns")
	
	write.table(x=tabout, sep="\t",col.names=T, row.names=F, qmethod="escape",quote=T,
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
	lpf = c()
	for(i in 1:length(txtfilesfp)){
		clen = countLines(txtfilesfp[i])
		lpf = c(lpf, clen)
		totalLines = totalLines + clen
		cat("file", txtfilesfp[i], "length", clen, "....\n")
	}
	cat("Warning: this function can only provide a conservative estimate of the number of lines in a file. It")
	cat("counts the number of lines in a text file by counting the number of occurances of platform-independent\n",
			"newlines (CR, LF, and CR+LF [1]), including a last line with neither. An empty file has zero lines.\n")
	
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


