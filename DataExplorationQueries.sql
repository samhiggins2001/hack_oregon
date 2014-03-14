"PO BOX 1341 SCAPPOOSE OR 97056" from fins, zip column :"97056"
13082

select * 
from fins
where
"Contributor_Payee_Committee_ID" = '13082'



select * from comms



select * 
from fins
where "Sub_Type" = 'Cash Contribution'


select * 
from fins
where "Sub_Type" = 'Cash Contribution'

"The Good Government Council";"";"Interest/Investment Income";"3.23";"0";"10367"
select * from comms
where "Committee_Type" = 'CC'


select * from "PAC"

select * from committee
where "Committee_Id" = 10367

select * from fins limit 100;

create table fundingTargets 
as select "Committee_Id", "Committee_Name"


select distinct "X_Book_Type_" from fins

select * from fins 
where "X_Contributor_Payee_committee_ID" != ''


select * from fins 
where "X_Purpose_Codes_" != ''
order by "X_Sub_Type_"

select distinct "X_Sub_Type_", count("X_Sub_Type_") from fins 
where "X_Purpose_Codes_" != ''
group by "X_Sub_Type_"
order by count("X_Sub_Type_")


select distinct "X_Sub_Type_", count("X_Sub_Type_") from fins 
group by "X_Sub_Type_"
order by count("X_Sub_Type_")


select * from fins 
where "X_Purpose_Codes_" != ''
and "X_Sub_Type_" = 


order by "X_Sub_Type_"


select distinct "X_Purpose_Codes_" from fins 


