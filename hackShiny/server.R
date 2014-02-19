print(getwd())
source('../openFiles.R')
# readFinData<-function(fname){
# 	tabin = read.table(file=fname,
# 										 strip.white=T,
# 										 comment.char="",
# 										 check.names=F,
# 										 header=T, 
# 										 sep="\t", 
# 										 stringsAsFactors=F)	
# 	return(tabin)
# }
fullTabName="/Users/samhiggins2001_worldperks/prog/hack_oregon/orestar/fins/joinedTables.txt"
fin = readFinData(fullTabName)

shinyServer(function(input, output) {
	
	output$main_plot <- reactivePlot(function() {
		
		hist(faithful$eruptions,
				 probability = TRUE,
				 breaks = as.numeric(input$n_breaks),
				 xlab = "Duration (minutes)",
				 main = "Geyser eruption duration")
		
		if (input$individual_obs) {
			rug(faithful$eruptions)
		}
		
		if (input$density) {
			dens <- density(faithful$eruptions,
											adjust = input$bw_adjust)
			lines(dens, col = "blue")
		}
		
	})
	output$finsTable<-renderDataTable({
		fin
	})
})