#openFiles3.R

finsxlsres = importAllXLSFiles(indir="../orestar/fins/problemSpreadsheets/", forceImport=T,
															 outsuffix=".txt")

res = mergeTxtFiles(folderName="../orestar/fins")


colsums = colSummaries(ftab=res)

write.table(x=colsums, sep="\t",col.names=T, row.names=F, qmethod="escape",quote=T,
						file="./commsColSums.txt")




dim(res)

which(res$Committee.Id=="")
