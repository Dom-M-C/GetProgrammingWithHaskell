
Drop table if exists Checkedout;
Drop table if exists Tool;
Drop table if exists Users;

Create Table [User]
(   Id Integer Primary Key
,   UserName Text Unique

,   Constraint UK_User_UserName Unique (UserName)
);

Create Table Tool
(   Id Integer Primary Key
,   Name Text Unique
,   Description Text
,   LastReturned Text
,   TimesBorrowed Integer
,   QuantityOwned Integer
);

Create Table Checkedout
(   UserId Integer
,   ToolId Integer
,   QuantityCheckedout Integer Not Null Default 0
        Check(QuantityCheckedout >= 0)

,   Constraint PK_Checkedout Primary Key(UserId, ToolId)
,   Constraint FK_Checkedout_User Foreign Key(UserId)
        REFERENCES [User](Id)
,   Constraint FK_Checkedout_Tool Foreign Key(ToolId)
        REFERENCES Tool(Id)
);

insert into [User] (UserName) values('willkurt');
insert into [User] (UserName) values('Dom');

insert into Tool (Name, Description, LastReturned, TimesBorrowed, QuantityOwned)
values('hammer', 'hits stuff', '2017-01-01', 0, 1);

insert into Tool (Name, Description, LastReturned, TimesBorrowed, QuantityOwned)
values('saw', 'cuts stuff', '2017-01-01', 0, 1);
