
fintab = read.table(file="./orestar/fins/txtOut/joinedTables.tsv", sep="\t", header=T, stringsAsFactors=F)

bttC = aggregate(x=fintab$Amount, by=list(fintab$Book.Type), FUN=sum)
colnames(bttC)<-c("name","value")

write.table(x=bttC, 
						file="./barChart/BookTypeData.tsv", 
						quote=F, 
						row.names=F, 
						col.names=T, 
						sep="\t")

code = aggregate(x=fintab$Amount, by=list(fintab$Purpose.Codes), FUN=sum)
colnames(code)<-c("name","value")

write.table(x=code, 
						file="./barChart/PurposeCodeData.tsv", 
						quote=F, 
						row.names=F, 
						col.names=T, 
						sep="\t")
