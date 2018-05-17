ALTER SESSION SET NLS_DATE_FORMAT = 'HH24:MI:SS';
drop table Tickets;
drop table Showtimes;
drop table Movies;
drop table Theaters;
drop table Users;

create table Users(
	userID int CHECK (userID > 0),
	email varchar2(30) NOT NULL,
	age varchar2(30) NOT NULL,
	numberBookedSoFar int,
	primary key(userID)
	);

create table Theaters(
	theaterID int CHECK (theaterID > 0),
	name varchar2(30) NOT NULL,
	address varchar2(25) NOT NULL,
	primary key(theaterID)
	);

create table Movies(
	movieID int CHECK (movieID > 0),
	title varchar2(30) NOT NULL,
	genre varchar2(25) NOT NULL,
	year int NOT NULL,
	runtime DATE NOT NULL, 
	primary key(movieID)
	);
	
create table Showtimes(
	showID int CHECK (showID > 0),
	movieID int NOT NULL,
	theaterID int NOT NULL,
	starttime DATE NOT NULL,
	endtime DATE NOT NULL,
	hall int NOT NULL,
	max_occupancy int NOT NULL,
	primary key(showID),
	foreign key(movieID) references Movies(movieID),
	foreign key(theaterID) references Theaters(theaterID)
	);
	
create table Tickets(
	userID int CHECK(userID > 0),
	showID int NOT NULL,
	primary key(userID,showID),
	foreign key(userID) references Users(userID),
	foreign key(showID) references Showtimes(showID)
);

insert into Users values(1,'jeff@abcd.com','Teen',5);
insert into Users values(2,'john@abcd.com','Young Adult',7);
insert into Users values(3,'amy@abcd.com','Adult',14);
insert into Users values(4,'adam@abcd.com','Middle-Aged',18);
insert into Users values(5,'buck@abcd.com','Senior',22);
insert into Users values(6,'beth@abcd.com','Teen',28);
insert into Users values(7,'nick@abcd.com','Young Adult',13);
insert into Users values(8,'nicole@abcd.com','Adult',16);
insert into Users values(9,'matt@abcd.com','Middle-Aged',6);
insert into Users values(10,'miley@abcd.com','Senior',2);

insert into Theaters values(1,'Goodrich','West Lafayette');
insert into Theaters values(2,'AMC','Indianapolis');
insert into Theaters values(3,'PVR','Lafayette');
insert into Theaters values(4,'IMAX','Chicago');

insert into Movies values(1,'The Great Wall','Fantasy',2016,'02:00:00');
insert into Movies values(2,'The Martian','SciFi',2015,'02:30:00');
insert into Movies values(3,'Inside Out','Animated',2015,'01:45:00');
insert into Movies values(4,'Interstellar','SciFi',2014,'02:45:00');
insert into Movies values(5,'Big Hero 6','Animated',2014,'01:30:00');
insert into Movies values(6,'Logan','Drama',2017,'02:15:00');

insert into Showtimes values(1,2,1,'13:30:00','16:00:00',1,100);
insert into Showtimes values(2,1,2,'14:00:00','16:00:00',1,90);
insert into Showtimes values(3,3,1,'14:30:00','16:15:00',2,80);
insert into Showtimes values(4,5,3,'15:00:00','16:30:00',1,70);
insert into Showtimes values(5,4,4,'15:30:00','18:15:00',1,10);
insert into Showtimes values(6,6,3,'15:30:00','17:15:00',2,70);
insert into Showtimes values(7,5,1,'15:00:00','17:45:00',3,80);
insert into Showtimes values(8,6,2,'14:30:00','16:45:00',2,90);
insert into Showtimes values(9,1,3,'14:00:00','16:00:00',3,100);
insert into Showtimes values(10,3,4,'13:30:00','15:15:00',2,110);

insert into Tickets values(1,1);
insert into Tickets values(2,2);
insert into Tickets values(3,3);
insert into Tickets values(4,4);
insert into Tickets values(5,5);
insert into Tickets values(6,6);
insert into Tickets values(7,7);
insert into Tickets values(8,8);
insert into Tickets values(9,9);
insert into Tickets values(10,10);

@setup
