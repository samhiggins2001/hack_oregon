#openFiles3.R

commsxlsres = importAllXLSFiles(indir="./orestar/comms", outsuffix=".txt")

res = mergeTxtFiles(folderName="./orestar/comms")


colsums = colSummaries(ftab=res)

write.table(x=colsums, sep="\t",col.names=T, row.names=F, qmethod="escape",quote=T,
						file="./commsColSums.txt")




dim(res)

which(res$Committee.Id=="")
