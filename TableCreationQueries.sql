alter table comms
alter column "Committee_Id" type text;

drop table"PAC";
create table "PAC"
as
select "Committee_Id", "Committee_Name","Committee_Type","Committee_SubType","Filing_Date","Organization_Filing_Date","Treasurer_First_Name","Treasurer_Last_Name",
"Treasurer_Mailing_Address","Treasurer_Fax","Measure"
from comms
where "Committee_Type" = 'PAC';

drop table "CC";
create table "CC" as
select "Committee_Id", "Committee_Name","Committee_Type","Candidate_Office","Candidate_Office_Group","Filing_Date","Organization_Filing_Date","Treasurer_First_Name","Treasurer_Last_Name",
"Treasurer_Mailing_Address","Treasurer_Fax", "Candidate_First_Name","Candidate_Last_Name","Candidate_Maling_Address","Candidate_Work_Phone","Candidate_Fax","Active_Election","Measure"
from comms
where "Committee_Type" = 'CC';

drop table committee;
create table committee 
as
select "Committee_Id", "Committee_Name","Committee_Type","Filing_Date","Organization_Filing_Date","Treasurer_First_Name","Treasurer_Last_Name",
"Treasurer_Mailing_Address","Treasurer_Fax","Measure"
from comms;

select * from fins limit 100
select * from comms


w
alter table fins
alter column "Amount" type numeric
	using "Amount"::numeric

alter table fins
alter column "Aggregate_Amount" type numeric
	using "Aggregate_Amount"::numeric

select * from fins limit 100


/*make a table of donating entities and, if possible,their ids*/
drop table donors;
create table donors as
select distinct "Contributor_Payee"as "name", "Contributor_Payee_Committee_ID" as "id", "Addr_Line1","City", "State","Zip", "Book_Type"
from fins;

select * from donors 
where "id" is not null
order by "name"

delete from donors 
where "name" is null;

update donors
set "id" = "name"
where "id" is null;

drop table udonors;
create table udonors
as
select distinct "name","id"
from donors;

select * from udonors;

delete from udonors
where "name" is null
