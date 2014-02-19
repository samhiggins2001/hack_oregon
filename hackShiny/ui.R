library("shiny")

shinyUI(bootstrapPage(
	dataTableOutput(outputId="finsTable")
))