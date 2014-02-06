#eda.R


fins = read.table(header=T, comment.char="",stringsAsFactors=F,
					 sep="\t",
					 file="./orestar/fins/joinedTables.tsv")


cat("Data dimensions:")
print(dim(fins))

colnames(fins)
# > colnames(fins)
# [1] "Tran.Id"                        "Original.Id"                   
# [3] "Tran.Date"                      "Tran.Status"                   
# [5] "Filer"                          "Contributor.Payee"             
# [7] "Sub.Type"                       "Amount"                        
# [9] "Aggregate.Amount"               "Contributor.Payee.Committee.ID"
# [11] "Filer.Id"                       "Attest.By.Name"                
# [13] "Attest.Date"                    "Review.By.Name"                
# [15] "Review.Date"                    "Due.Date"                      
# [17] "Occptn.Ltr.Date"                "Pymt.Sched.Txt"                
# [19] "Purp.Desc"                      "Intrst.Rate"                   
# [21] "Check.Nbr"                      "Tran.Stsfd.Ind"                
# [23] "Filed.By.Name"                  "Filed.Date"                    
# [25] "Addr.book.Agent.Name"           "Book.Type"                     
# [27] "Title.Txt"                      "Occptn.Txt"                    
# [29] "Emp.Name"                       "Emp.City"                      
# [31] "Emp.State"                      "Employ.Ind"                    
# [33] "Self.Employ.Ind"                "Addr.Line1"                    
# [35] "Addr.Line2"                     "City"                          
# [37] "State"                          "Zip"                           
# [39] "Zip.Plus.Four"                  "County"                        
# [41] "Purpose.Codes"                  "Exp.Date" 
summary(object=fins)

tabsums = list()

for(c in colnames(fins)){
	cat("\n\nThis column:", c,"\n")
	tabsums[[c]] = as.data.frame(as.matrix(table(fins[[c]])))#table(fins[[c]])
	print(length(tabsums[[c]]))
	if(nrow(tabsums[[c]])<100) print( tabsums[[c]] )
}




