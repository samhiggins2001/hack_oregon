library("shiny", lib.loc="~/rpackages/")

shinyUI(bootstrapPage(
	dataTableOutput(outputId="finsTable")
))