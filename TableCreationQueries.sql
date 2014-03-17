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

select * from fins limit 1000
select * from comms


alter table fins
alter column "Amount" type numeric
	using "Amount"::numeric

alter table fins
alter column "Aggregate_Amount" type numeric
	using "Aggregate_Amount"::numeric

select * from fins limit 100

drop table donors1;
create table donors as
select "Contributor_Payee"as "name", "Contributor_Payee_Committee_ID" as "id", "Addr_Line1","City", "State","Zip", "Book_Type"
from fins;

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

select "Book_Type", count("Tran_Id") from fins group by "Book_Type"


limit 100

select * from comms limit 100

select * from fins where "Tran_Id" = '97298'


drop table sif1;
create table sif1 as
select "Tran_Id","Tran_Date", "Filer_Id" as "comm_id", "Contributor_Payee_Committee_ID" as "other_entity", "Sub_Type" as "type", "Contributor_Payee" as "other_entitys_name", "Amount"
from fins;

update sif1
set "other_entity" = "other_entitys_name" 
where "other_entity" is null;

update sif1
set "other_entity" = "type"
where "other_entity" is null

select * from sif1 limit 1000;

select "Sub_Type", count("Tran_Id"), sum("Amount") from fins 
where "Contributor_Payee" is null
group by "Sub_Type"

select * 
from fins 
where "Tran_Id" 
in
	(select "Tran_Id" 
	from sif1 
	where "other_entity" is null)


/*make a unique id-name mapper: 
	first get the list of committee names and ids from the comms table
	second, get the list of */


select * from fins 
where "Contributor_Payee_Committee_ID" 
not in 	(select "Committee_Id" 
	from comms)
and "Contributor_Payee_Committee_ID" is not null
/*6821 rows*/

select * from fins 
where "Contributor_Payee_Committee_ID" 
in 	(select "Committee_Id" 
	from comms)
and "Contributor_Payee_Committee_ID" is not null
/*56538 rows*/


select * from ssif 
where money_from in 
	(select distinct "Sub_Type" from fins)

