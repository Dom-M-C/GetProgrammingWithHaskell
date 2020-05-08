
Drop table if exists Checkedout;
Drop table if exists Tool;
Drop table if exists Users;

Create table [User]
(   Id Integer Primary Key
,   UserName Text

,   CONSTRAINT UK_User_UserName UNIQUE (UserName)
);

Create table Tool
(   Id Integer Primary Key
,   Name Text UNIQUE
,   Description Text
,   LastReturned Text
,   TimesBorrowed Integer
);

Create table Checkedout
(   UserId Integer
,   ToolId Integer
);

insert into [User] (UserName) values('willkurt');
insert into [User] (UserName) values('Dom');

insert into Tool (Name, Description, LastReturned, TimesBorrowed)
values('hammer', 'hits stuff', '2017-01-01', 0);

insert into Tool (Name, Description, LastReturned, TimesBorrowed)
values('saw', 'cuts stuff', '2017-01-01', 0);
