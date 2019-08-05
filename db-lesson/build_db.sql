
Drop table if exists Checkedout;
Drop table if exists Tool;
Drop table if exists Users;

Create table Users
(   Id Integer Primary Key
,   UserName Text
);

Create table Tool
(   Id Integer Primary Key
,   Name Text
,   Description Text
,   LastReturned Text
,   TimesBorrowed Integer
);

Create table Checkedout
(   UserId Integer
,   ToolId Integer
);

insert into Users (UserName) values('willkurt')
insert into Users (UserName) values('Dom')

insert into Tool (Name, Description, LastReturned, TimesBorrowed)
values('hammer', 'hits stuff', '2017-01-01', 0)

insert into Tool (Name, Description, LastReturned, TimesBorrowed)
values('saw', 'cuts stuff', '2017-01-01', 0)
