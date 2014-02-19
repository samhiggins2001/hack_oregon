# print(getwd())
source('./openFiles.R')

fullTabName="./orestar/fins/joinedTables.txt"
fin = readFinData(fullTabName)

shinyServer(function(input, output) {
	
	output$finsTable<-renderDataTable({
		fin
	})
})