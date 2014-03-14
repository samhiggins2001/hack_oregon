#dataDictionaryScripts.R


tmp = mergeTxtFiles()

fullTabName="/Users/samhiggins2001_worldperks/prog/hack_oregon/orestar/fins/joinedTables.txt"


fin = readFinData(fname=fullTabName)

comTabName = "/Users/samhiggins2001_worldperks/prog/hack_oregon/orestar/comms/2002.xls"

comtab = readFinData(fname=fullTabName)
contDat = colSummaries(ftab=comtab)



ftest = fin$Filer[1]

unique(fin$Contributor.Payee==ftest)

sum(is.na(fin$Contributor.Payee))

colnames(fin)
[1] "Tran.Id"  : transaction id                     
[2] "Original.Id"   : cursory glance shows it to be the same as the Tran.Id                
[3] "Tran.Date"     : contribution date
[4] "Tran.Status"   : levels: original or ammended ... not sure what this one is               
[5] "Filer"         : the entity recieving the funds?
[6] "Contributor.Payee"   : three of these are NA ; otherwise, I believe this is the group recieving money          
[7] "Sub.Type"            : the type of contribution? 31 levels:
	Account Payable              Account Payable Rescinded 
2                                   6794                                     98 
Cash Balance Adjustment                      Cash Contribution                       Cash Expenditure 
323                                 250178                                 166840 
Expenditure Made by an Agent                   In-Kind Contribution       In-Kind/Forgiven Account Payable 
1926                                  17567                                     58 
In-Kind/Forgiven Personal Expenditures             Interest/Investment Income        Items Sold at Fair Market Value 
208                                   5202                                  13953 
Loan Forgiven (Non-Exempt)                  Loan Payment (Exempt)              Loan Payment (Non-Exempt) 
101                                     16                                    406 
Loan Received (Exempt)             Loan Received (Non-Exempt)                 Lost or Returned Check 
11                                    789                                   1065 
Miscellaneous Account Receivable       Miscellaneous Other Disbursement            Miscellaneous Other Receipt 
157                                   1160                                   8060 
Nonpartisan Activity Personal Expenditure for Reimbursement                         Pledge of Cash 
39                                  28436                                   2335 
Pledge of In-Kind                         Pledge of Loan                    Refunds and Rebates 
149                                      4                                   1410 
Return or Refund of Contribution           Uncollectible Pledge of Cash        Uncollectible Pledge of In-Kind 
734                                    119                                      1 
Unexpended Agent Balance 
21 
longTextBarPlot(data=table(fin$Sub.Type), lab=names(table(fin$Sub.Type)), main="Different values in the \"Sub.Type column\"")
[8] "Amount"                        :the amount contributed
[9] "Aggregate.Amount"              :? sometimes more than the "Amount" column, sometimes less....?
[10] "Contributor.Payee.Committee.ID": the ID of the
[11] "Filer.Id"                      
[12] "Attest.By.Name"                
[13] "Attest.Date"                   
[14] "Review.By.Name"                
[15] "Review.Date"                   
[16] "Due.Date"                      
[17] "Occptn.Ltr.Date"               
[18] "Pymt.Sched.Txt"                
[19] "Purp.Desc"                     
[20] "Intrst.Rate"                   
[21] "Check.Nbr"                     
[22] "Tran.Stsfd.Ind"                
[23] "Filed.By.Name"                 
[24] "Filed.Date"                    
[25] "Addr.book.Agent.Name"          
[26] "Book.Type"                     
[27] "Title.Txt"                     
[28] "Occptn.Txt"                    
[29] "Emp.Name"                      
[30] "Emp.City"                      
[31] "Emp.State"                     
[32] "Employ.Ind"                    
[33] "Self.Employ.Ind"               
[34] "Addr.Line1"                    
[35] "Addr.Line2"                    
[36] "City"                          
[37] "State"                         
[38] "Zip"                           
[39] "Zip.Plus.Four"                 
[40] "County"                        
[41] "Purpose.Codes"                 
[42] "Exp.Date"
