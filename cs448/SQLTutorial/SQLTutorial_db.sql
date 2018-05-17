-- Table Creation
--
--		This file will create the tables for use with the book 
--  Database Management Systems by Raghu Ramakrishnan and Johannes Gehrke.
--  It is run automatically by the installation script.
--
--	Version 0.1.0.0 2002/04/05 by: David Warden.
--	Copyright (C) 2002 McGraw-Hill Companies Inc. All Rights Reserved.
create table student(
	snum number(9,0) primary key,
	sname varchar2(30),
	major varchar2(25),
	standing varchar2(2),
	age number(3,0)
	);
create table faculty(
	fid number(9,0) primary key,
	fname varchar2(30),
	deptid number(2,0)
	);
create table class(
	name varchar2(40) primary key,
	meets_at varchar2(20),
	room varchar2(10),
	fid number(9,0),
	foreign key(fid) references faculty
	);
create table enrolled(
	snum number(9,0),
	cname varchar2(40),
	primary key(snum,cname),
	foreign key(snum) references student,
	foreign key(cname) references class(name)
	);
create table emp(
	eid number(9,0) primary key,
	ename varchar2(30),
	age number(3,0),
	salary number(10,2)
	);
create table dept(
	did number(2,0) primary key,
	dname varchar2(20),
	budget number(10,2),
	managerid number(9,0),
	foreign key(managerid) references emp(eid)
	);
create table works(
	eid number(9,0),
	did number(2,0),
	pct_time number(3,0),
	primary key(eid,did),
	foreign key(eid) references emp,
	foreign key(did) references dept
	);
create table flights(
	flno number(4,0) primary key,
	origin varchar2(20),
	destination varchar2(20),
	distance number(6,0),
	departs date,
	arrives date,
	price number(7,2)
	);
create table aircraft(
	aid number(9,0) primary key,
	aname varchar2(30),
	crusingrange number(6,0)
	);
create table employees(
	eid number(9,0) primary key,
	ename varchar2(30),
	salary number(10,2)
	);
create table certified(
	eid number(9,0),
	aid number(9,0),
	primary key(eid,aid),
	foreign key(eid) references employees,
	foreign key(aid) references aircraft
	);
create table suppliers(
	sid number(9,0) primary key,
	sname varchar2(30),
	address varchar2(45)
	);
create table parts(
	pid number(9,0) primary key,
	pname varchar2(40),
	color varchar2(15)
	);
create table catalog(
	sid number(9,0),
	pid number(9,0),
	cost number(10,2),
	primary key(sid,pid),
	foreign key(sid) references suppliers,
	foreign key(pid) references parts
	);
create table sailors(
	sid number(9,0) primary key,
	sname varchar2(30),
	rating number(2,0),
	age number(4,1)
	);

/*******************
 * POPULATE TABLES *
 *******************/
--student
INSERT INTO student VALUES(051135593,'Maria White','English','SR',21);
INSERT INTO student VALUES(060839453,'Charles Harris','Architecture','SR',22);
INSERT INTO student VALUES(099354543,'Susan Martin','Law','JR',20);
INSERT INTO student VALUES(112348546,'Joseph Thompson','Computer Science','SO',19);
INSERT INTO student VALUES(115987938,'Christopher Garcia','Computer Science','JR',20);
INSERT INTO student VALUES(132977562,'Angela Martinez','History','SR',20);
INSERT INTO student VALUES(269734834,'Thomas Robinson','Psychology','SO',18);
INSERT INTO student VALUES(280158572,'Margaret Clark','Animal Science','FR',18);
INSERT INTO student VALUES(301221823,'Juan Rodriguez','Psychology','JR',20);
INSERT INTO student VALUES(318548912,'Dorthy Lewis','Finance','FR',18);
INSERT INTO student VALUES(320874981,'Daniel Lee','Electrical Engineering','FR',17);
INSERT INTO student VALUES(322654189,'Lisa Walker','Computer Science','SO',17);
INSERT INTO student VALUES(348121549,'Paul Hall','Computer Science','JR',18);
INSERT INTO student VALUES(351565322,'Nancy Allen','Accounting','JR',19);
INSERT INTO student VALUES(451519864,'Mark Young','Finance','FR',18);
INSERT INTO student VALUES(455798411,'Luis Hernandez','Electrical Engineering','FR',17);
INSERT INTO student VALUES(462156489,'Donald King','Mechanical Engineering','SO',19);
INSERT INTO student VALUES(550156548,'George Wright','Education','SR',21);
INSERT INTO student VALUES(552455318,'Ana Lopez','Computer Engineering','SR',19);
INSERT INTO student VALUES(556784565,'Kenneth Hill','Civil Engineering','SR',21);
INSERT INTO student VALUES(567354612,'Karen Scott','Computer Engineering','FR',18);
INSERT INTO student VALUES(573284895,'Steven Green','Kinesiology','SO',19);
INSERT INTO student VALUES(574489456,'Betty Adams','Economics','JR',20);
INSERT INTO student VALUES(578875478,'Edward Baker','Veterinary Medicine','SR',21);
--faculty
INSERT INTO faculty VALUES(142519864,'Ivana Teach',20);
INSERT INTO faculty VALUES(242518965,'James Smith',68);
INSERT INTO faculty VALUES(141582651,'Mary Johnson',20);
INSERT INTO faculty VALUES(011564812,'John Williams',68);
INSERT INTO faculty VALUES(254099823,'Patricia Jones',68);
INSERT INTO faculty VALUES(356187925,'Robert Brown',12);
INSERT INTO faculty VALUES(489456522,'Linda Davis',20);
INSERT INTO faculty VALUES(287321212,'Michael Miller',12);
INSERT INTO faculty VALUES(248965255,'Barbara Wilson',12);
INSERT INTO faculty VALUES(159542516,'William Moore',33);
INSERT INTO faculty VALUES(090873519,'Elizabeth Taylor',11);
INSERT INTO faculty VALUES(486512566,'David Anderson',20);
INSERT INTO faculty VALUES(619023588,'Jennifer Thomas',11);
INSERT INTO faculty VALUES(489221823,'Richard Jackson',33);
INSERT INTO faculty VALUES(548977562,'Ulysses Teach',20);
--class
INSERT INTO class VALUES('Data Structures','MWF 10','R128',489456522);
INSERT INTO class VALUES('Database Systems','MWF 12:30-1:45','1320 DCL',142519864);
INSERT INTO class VALUES('Operating System Design','TuTh 12-1:20','20 AVW',489456522 );
INSERT INTO class VALUES('Archaeology of the Incas','MWF 3-4:15','R128',248965255);
INSERT INTO class VALUES('Aviation Accident Investigation','TuTh 1-2:50','Q3',011564812);
INSERT INTO class VALUES('Air Quality Engineering','TuTh 10:30-11:45','R15',011564812);
INSERT INTO class VALUES('Introductory Latin','MWF 3-4:15','R12',248965255);
INSERT INTO class VALUES('American Political Parties','TuTh 2-3:15','20 AVW',619023588);
INSERT INTO class VALUES('Social Cognition','Tu 6:30-8:40','R15',159542516);
INSERT INTO class VALUES('Perception','MTuWTh 3','Q3',489221823);
INSERT INTO class VALUES('Multivariate Analysis','TuTh 2-3:15','R15',090873519);
INSERT INTO class VALUES('Patent Law','F 1-2:50','R128',090873519);
INSERT INTO class VALUES('Urban Economics','MWF 11','20 AVW',489221823);
INSERT INTO class VALUES('Organic Chemistry','TuTh 12:30-1:45','R12',489221823);
INSERT INTO class VALUES('Marketing Research','MW 10-11:15','1320 DCL',489221823);
INSERT INTO class VALUES('Seminar in American Art','M 4','R15',489221823);
INSERT INTO class VALUES('Orbital Mechanics','MWF 8','1320 DCL',011564812);
INSERT INTO class VALUES('Dairy Herd Management','TuTh 12:30-1:45','R128',356187925);
INSERT INTO class VALUES('Communication Networks','MW 9:30-10:45','20 AVW',141582651);
INSERT INTO class VALUES('Optical Electronics','TuTh 12:30-1:45','R15',254099823);
INSERT INTO class VALUES('Intoduction to Math','TuTh 8-9:30','R128',489221823);
--enrolled
INSERT INTO enrolled VALUES(112348546,'Database Systems');
INSERT INTO enrolled VALUES(115987938,'Database Systems');
INSERT INTO enrolled VALUES(348121549,'Database Systems');
INSERT INTO enrolled VALUES(322654189,'Database Systems');
INSERT INTO enrolled VALUES(552455318,'Database Systems');
INSERT INTO enrolled VALUES(455798411,'Operating System Design');
INSERT INTO enrolled VALUES(552455318,'Operating System Design');
INSERT INTO enrolled VALUES(567354612,'Operating System Design');
INSERT INTO enrolled VALUES(112348546,'Operating System Design');
INSERT INTO enrolled VALUES(115987938,'Operating System Design');
INSERT INTO enrolled VALUES(322654189,'Operating System Design');
INSERT INTO enrolled VALUES(567354612,'Data Structures');
INSERT INTO enrolled VALUES(552455318,'Communication Networks');
INSERT INTO enrolled VALUES(455798411,'Optical Electronics');
INSERT INTO enrolled VALUES(301221823,'Perception');
INSERT INTO enrolled VALUES(301221823,'Social Cognition');
INSERT INTO enrolled VALUES(301221823,'American Political Parties');
INSERT INTO enrolled VALUES(556784565,'Air Quality Engineering');
INSERT INTO enrolled VALUES(099354543,'Patent Law');
INSERT INTO enrolled VALUES(574489456,'Urban Economics');
--emp
INSERT INTO emp VALUES(142519864,'Susan Martin',39,56990);
INSERT INTO emp VALUES(242518965,'James Smith',68,27099);
INSERT INTO emp VALUES(141582651,'Mary Johnson',44,94011);
INSERT INTO emp VALUES(011564812,'John Williams',35,74098);
INSERT INTO emp VALUES(254099823,'Patricia Jones',28,42783);
INSERT INTO emp VALUES(356187925,'Robert Brown',28,35431);
INSERT INTO emp VALUES(489456522,'Linda Davis',26,25971);
INSERT INTO emp VALUES(287321212,'Michael Miller',62,131072);
INSERT INTO emp VALUES(248965255,'Barbara Wilson',48,95021);
INSERT INTO emp VALUES(159542516,'Matt Nelson',33,48990);
INSERT INTO emp VALUES(090873519,'Elizabeth Taylor',27,33055);
INSERT INTO emp VALUES(486512566,'David Anderson',20,25199);
INSERT INTO emp VALUES(619023588,'Jennifer Thomas',24,34654);
INSERT INTO emp VALUES(112348546,'Joseph Thompson',26,24998);
INSERT INTO emp VALUES(115987938,'Christopher Garcia',60,24998);
INSERT INTO emp VALUES(132977562,'Angela Martinez',31,24998);
INSERT INTO emp VALUES(269734834,'Rick Carter',38,24998);
INSERT INTO emp VALUES(280158572,'Margaret Clark',40,24998);
INSERT INTO emp VALUES(301221823,'Juan Rodriguez',30,32175);
INSERT INTO emp VALUES(318548912,'Ann Mitchell',23,32175);
INSERT INTO emp VALUES(320874981,'Daniel Lee',23,32175);
INSERT INTO emp VALUES(322654189,'Lisa Walker',38,32175);
INSERT INTO emp VALUES(348121549,'Trey Phillips',69,32175);
INSERT INTO emp VALUES(351565322,'Nancy Allen',30,39910);
INSERT INTO emp VALUES(451519864,'Mark Young',34,39910);
INSERT INTO emp VALUES(455798411,'Luis Hernandez',44,39910);
INSERT INTO emp VALUES(550156548,'George Wright',42,41008);
INSERT INTO emp VALUES(552455318,'Ana Lopez',35,41008);
INSERT INTO emp VALUES(556784565,'Kenneth Hill',81,41008);
INSERT INTO emp VALUES(567354612,'Karen Scott',70,39910);
INSERT INTO emp VALUES(573284895,'Steven Green',29,39910);
INSERT INTO emp VALUES(574489456,'Betty Adams',39,39910);
INSERT INTO emp VALUES(015645489,'Daniel Evans',25,40312);
INSERT INTO emp VALUES(015487874,'Gene Edwards',51,41008);
INSERT INTO emp VALUES(054879887,'Dorthy Lewis',33,41008);
INSERT INTO emp VALUES(098784544,'Eric Collins',23,41008);
INSERT INTO emp VALUES(074454898,'Scott Bell',23,70100);
INSERT INTO emp VALUES(156489494,'Gil Richardson',32,70100);
INSERT INTO emp VALUES(179887498,'Dorthy Howard',28,40312);
INSERT INTO emp VALUES(156465461,'Eric Cooper',26,24998);
INSERT INTO emp VALUES(128778823,'William Ward',33,24998);
INSERT INTO emp VALUES(178949844,'Chad Stewart',29,24998);
INSERT INTO emp VALUES(298489484,'Lisa Gray',31,24998);
INSERT INTO emp VALUES(274878974,'Harry Watson',30,24998);
INSERT INTO emp VALUES(267894232,'Paul Hall',25,24998);
INSERT INTO emp VALUES(254898318,'Gim Rogers',25,32175);
INSERT INTO emp VALUES(489221823,'Richard Jackson',33,32996);
INSERT INTO emp VALUES(548977562,'Donald King',43,92048);
INSERT INTO emp VALUES(289562686,'Thomas Robinson',34,32175);
INSERT INTO emp VALUES(291795563,'Haywood Kelly',36,32175);
INSERT INTO emp VALUES(578875478,'Edward Baker',50,101071);
INSERT INTO emp VALUES(051135593,'Maria White',22,24998);
INSERT INTO emp VALUES(060839453,'Charles Harris',24,24998);
INSERT INTO emp VALUES(334568786,'William Moore',32,32175);
INSERT INTO emp VALUES(355548984,'Tom Murphy',41,32175);
INSERT INTO emp VALUES(310454876,'Milo Brooks',22,39910);
INSERT INTO emp VALUES(390487451,'Mark Coleman',42,39910);
INSERT INTO emp VALUES(454565232,'Louis Jenkins',20,39910);
INSERT INTO emp VALUES(141582657,'Stanley Browne',23,14093);
--dept
INSERT INTO dept VALUES(1,'Hardware',1048572.12,141582651);
INSERT INTO dept VALUES(2,'Operations',12099101.00,287321212);
INSERT INTO dept VALUES(3,'Legal',222988.13,248965255);
INSERT INTO dept VALUES(4,'Marketing',538099.54,548977562);
INSERT INTO dept VALUES(5,'Software',400011.12,141582651);
INSERT INTO dept VALUES(6,'Production',12099101.00,578875478);
INSERT INTO dept VALUES(7,'Shipping',5.00,489456522);
--works
INSERT INTO works VALUES(142519864,2,100);
INSERT INTO works VALUES(242518965,1,100);
INSERT INTO works VALUES(141582651,1,50);
INSERT INTO works VALUES(141582651,5,50);
INSERT INTO works VALUES(141582657,1,25);
INSERT INTO works VALUES(141582657,5,75);
INSERT INTO works VALUES(011564812,3,100);
INSERT INTO works VALUES(254099823,3,100);
INSERT INTO works VALUES(356187925,2,100);
INSERT INTO works VALUES(489456522,7,100);
INSERT INTO works VALUES(287321212,2,100);
INSERT INTO works VALUES(248965255,3,100);
INSERT INTO works VALUES(159542516,4,100);
INSERT INTO works VALUES(090873519,2,100);
INSERT INTO works VALUES(486512566,4,100);
INSERT INTO works VALUES(619023588,1,100);
INSERT INTO works VALUES(489221823,2,100);
INSERT INTO works VALUES(548977562,4,100);
INSERT INTO works VALUES(578875478,6,100);
INSERT INTO works VALUES(051135593,2,100);
INSERT INTO works VALUES(060839453,2,100);
INSERT INTO works VALUES(112348546,2,100);
INSERT INTO works VALUES(115987938,2,100);
INSERT INTO works VALUES(132977562,2,100);
INSERT INTO works VALUES(269734834,2,100);
INSERT INTO works VALUES(280158572,2,100);
INSERT INTO works VALUES(301221823,2,100);
INSERT INTO works VALUES(318548912,2,100);
INSERT INTO works VALUES(320874981,2,100);
INSERT INTO works VALUES(322654189,2,100);
INSERT INTO works VALUES(348121549,2,100);
INSERT INTO works VALUES(351565322,2,100);
INSERT INTO works VALUES(451519864,2,100);
INSERT INTO works VALUES(455798411,2,100);
INSERT INTO works VALUES(550156548,2,50);
INSERT INTO works VALUES(552455318,2,25);
INSERT INTO works VALUES(556784565,2,25);
INSERT INTO works VALUES(567354612,2,75);
INSERT INTO works VALUES(573284895,2,50);
INSERT INTO works VALUES(574489456,2,50);
INSERT INTO works VALUES(015645489,6,100);
INSERT INTO works VALUES(015487874,6,100);
INSERT INTO works VALUES(054879887,6,100);
INSERT INTO works VALUES(098784544,6,100);
INSERT INTO works VALUES(074454898,6,100);
INSERT INTO works VALUES(156489494,6,100);
INSERT INTO works VALUES(179887498,6,100);
INSERT INTO works VALUES(156465461,6,100);
INSERT INTO works VALUES(128778823,6,100);
INSERT INTO works VALUES(178949844,6,100);
INSERT INTO works VALUES(298489484,6,100);
INSERT INTO works VALUES(274878974,6,100);
INSERT INTO works VALUES(267894232,6,100);
INSERT INTO works VALUES(254898318,6,100);
INSERT INTO works VALUES(289562686,6,100);
INSERT INTO works VALUES(291795563,6,100);
INSERT INTO works VALUES(334568786,6,100);
INSERT INTO works VALUES(355548984,6,100);
INSERT INTO works VALUES(310454876,6,100);
INSERT INTO works VALUES(390487451,6,100);
INSERT INTO works VALUES(454565232,6,50);
--flights
INSERT INTO flights VALUES(99,'Los Angeles','Washington D.C.',2308,TO_DATE('2005/04/12 09:30', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 21:40', 'YYYY/mm/dd HH24:MI'),235.98);
INSERT INTO flights VALUES(13,'Los Angeles','Chicago',1749,TO_DATE('2005/04/12 08:45', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 20:45', 'YYYY/mm/dd HH24:MI'),220.98);
INSERT INTO flights VALUES(346,'Los Angeles','Dallas',1251,TO_DATE('2005/04/12 11:50', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 19:05', 'YYYY/mm/dd HH24:MI'),225.43);
INSERT INTO flights VALUES(387,'Los Angeles','Boston',2606,TO_DATE('2005/04/12 07:03', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 17:03', 'YYYY/mm/dd HH24:MI'),261.56);
INSERT INTO flights VALUES(7,'Los Angeles','Sydney',7487,TO_DATE('2005/04/12 22:30', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/14 6:10', 'YYYY/mm/dd HH24:MI'),1278.56);
INSERT INTO flights VALUES(2,'Los Angeles','Tokyo',5478,TO_DATE('2005/04/12 12:30', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/13 15:55', 'YYYY/mm/dd HH24:MI'),780.99);
INSERT INTO flights VALUES(33,'Los Angeles','Honolulu',2551,TO_DATE('2005/04/12 09:15', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 11:15', 'YYYY/mm/dd HH24:MI'),375.23);
INSERT INTO flights VALUES(34,'Los Angeles','Honolulu',2551,TO_DATE('2005/04/12 12:45', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 15:18', 'YYYY/mm/dd HH24:MI'),425.98);
INSERT INTO flights VALUES(76,'Chicago','Los Angeles',1749,TO_DATE('2005/04/12 08:32', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 10:03', 'YYYY/mm/dd HH24:MI'),220.98);
INSERT INTO flights VALUES(68,'Chicago','New York',802,TO_DATE('2005/04/12 09:00', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 12:02', 'YYYY/mm/dd HH24:MI'),202.45);
INSERT INTO flights VALUES(7789,'Madison','Detroit',319,TO_DATE('2005/04/12 06:15', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 08:19', 'YYYY/mm/dd HH24:MI'),120.33);
INSERT INTO flights VALUES(701,'Detroit','New York',470,TO_DATE('2005/04/12 08:55', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 10:26', 'YYYY/mm/dd HH24:MI'),180.56);
INSERT INTO flights VALUES(702,'Madison','New York',789,TO_DATE('2005/04/12 07:05', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 10:12', 'YYYY/mm/dd HH24:MI'),202.34);
INSERT INTO flights VALUES(4884,'Madison','Chicago',84,TO_DATE('2005/04/12 22:12', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 23:02', 'YYYY/mm/dd HH24:MI'),112.45);
INSERT INTO flights VALUES(2223,'Madison','Pittsburgh',517,TO_DATE('2005/04/12 08:02', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 10:01', 'YYYY/mm/dd HH24:MI'),189.98);
INSERT INTO flights VALUES(5694,'Madison','Minneapolis',247,TO_DATE('2005/04/12 08:32', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 09:33', 'YYYY/mm/dd HH24:MI'),120.11);
INSERT INTO flights VALUES(304,'Minneapolis','New York',991,TO_DATE('2005/04/12 10:00', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 1:39', 'YYYY/mm/dd HH24:MI'),101.56);
INSERT INTO flights VALUES(149,'Pittsburgh','New York',303,TO_DATE('2005/04/12 09:42', 'YYYY/mm/dd HH24:MI'),TO_DATE('2005/04/12 12:09', 'YYYY/mm/dd HH24:MI'),116.50);
--aircraft
INSERT INTO aircraft VALUES(1,'Boeing 747-400',8430);
INSERT INTO aircraft VALUES(2,'Boeing 737-800',3383);
INSERT INTO aircraft VALUES(3,'Airbus A340-300',7120);
INSERT INTO aircraft VALUES(4,'British Aerospace Jetstream 41',1502);
INSERT INTO aircraft VALUES(5,'Embraer ERJ-145',1530);
INSERT INTO aircraft VALUES(6,'SAAB 340',2128);
INSERT INTO aircraft VALUES(7,'Piper Archer III',520);
INSERT INTO aircraft VALUES(8,'Tupolev 154',4103);
INSERT INTO aircraft VALUES(16,'Schwitzer 2-33',30);
INSERT INTO aircraft VALUES(9,'Lockheed L1011',6900);
INSERT INTO aircraft VALUES(10,'Boeing 757-300',4010);
INSERT INTO aircraft VALUES(11,'Boeing 777-300',6441);
INSERT INTO aircraft VALUES(12,'Boeing 767-400ER',6475);
INSERT INTO aircraft VALUES(13,'Airbus A320',2605);
INSERT INTO aircraft VALUES(14,'Airbus A319',1805);
INSERT INTO aircraft VALUES(15,'Boeing 727',1504);
--employees
INSERT INTO employees VALUES(242518965,'James Smith',120433);
INSERT INTO employees VALUES(141582651,'Mary Johnson',178345);
INSERT INTO employees VALUES(011564812,'John Williams',153972);
INSERT INTO employees VALUES(567354612,'Lisa Walker',256481);
INSERT INTO employees VALUES(552455318,'Larry West',101745);
INSERT INTO employees VALUES(550156548,'Karen Scott',205187);
INSERT INTO employees VALUES(390487451,'Lawrence Sperry',212156);
INSERT INTO employees VALUES(274878974,'Michael Miller',99890);
INSERT INTO employees VALUES(254099823,'Patricia Jones',24450);
INSERT INTO employees VALUES(356187925,'Robert Brown',44740);
INSERT INTO employees VALUES(355548984,'Angela Martinez',212156 );
INSERT INTO employees VALUES(310454876,'Joseph Thompson',212156);
INSERT INTO employees VALUES(489456522,'Linda Davis',127984);
INSERT INTO employees VALUES(489221823,'Richard Jackson',23980);
INSERT INTO employees VALUES(548977562,'William Ward',84476);
INSERT INTO employees VALUES(310454877,'Chad Stewart',33546);
INSERT INTO employees VALUES(142519864,'Betty Adams',227489);
INSERT INTO employees VALUES(269734834,'George Wright',289950);
INSERT INTO employees VALUES(287321212,'Michael Miller',48090);
INSERT INTO employees VALUES(552455348,'Dorthy Lewis',92013);
INSERT INTO employees VALUES(248965255,'Barbara Wilson',43723);
INSERT INTO employees VALUES(159542516,'William Moore',48250);
INSERT INTO employees VALUES(348121549,'Haywood Kelly',32899);
INSERT INTO employees VALUES(090873519,'Elizabeth Taylor',32021);
INSERT INTO employees VALUES(486512566,'David Anderson',743001);
INSERT INTO employees VALUES(619023588,'Jennifer Thomas',54921);
INSERT INTO employees VALUES(015645489,'Donald King',18050);
INSERT INTO employees VALUES(556784565,'Mark Young',205187);
INSERT INTO employees VALUES(573284895,'Eric Cooper',114323);
INSERT INTO employees VALUES(574489456,'William Jones',105743);
INSERT INTO employees VALUES(574489457,'Milo Brooks',20);
--certified
INSERT INTO certified VALUES(567354612,1);
INSERT INTO certified VALUES(567354612,2);
INSERT INTO certified VALUES(567354612,10);
INSERT INTO certified VALUES(567354612,11);
INSERT INTO certified VALUES(567354612,12);
INSERT INTO certified VALUES(567354612,15);
INSERT INTO certified VALUES(567354612,7);
INSERT INTO certified VALUES(567354612,9);
INSERT INTO certified VALUES(567354612,3);
INSERT INTO certified VALUES(567354612,4);
INSERT INTO certified VALUES(567354612,5);
INSERT INTO certified VALUES(552455318,2);
INSERT INTO certified VALUES(552455318,14);
INSERT INTO certified VALUES(550156548,1);
INSERT INTO certified VALUES(550156548,12);
INSERT INTO certified VALUES(390487451,3);
INSERT INTO certified VALUES(390487451,13);
INSERT INTO certified VALUES(390487451,14);
INSERT INTO certified VALUES(274878974,10);
INSERT INTO certified VALUES(274878974,12);
INSERT INTO certified VALUES(355548984,8);
INSERT INTO certified VALUES(355548984,9);
INSERT INTO certified VALUES(310454876,8);
INSERT INTO certified VALUES(310454876,9);
INSERT INTO certified VALUES(548977562,7);
INSERT INTO certified VALUES(142519864,1);
INSERT INTO certified VALUES(142519864,11);
INSERT INTO certified VALUES(142519864,12);
INSERT INTO certified VALUES(142519864,10);
INSERT INTO certified VALUES(142519864,3);
INSERT INTO certified VALUES(142519864,2);
INSERT INTO certified VALUES(142519864,13);
INSERT INTO certified VALUES(142519864,7);
INSERT INTO certified VALUES(269734834,1);
INSERT INTO certified VALUES(269734834,2);
INSERT INTO certified VALUES(269734834,3);
INSERT INTO certified VALUES(269734834,4);
INSERT INTO certified VALUES(269734834,5);
INSERT INTO certified VALUES(269734834,6);
INSERT INTO certified VALUES(269734834,7);
INSERT INTO certified VALUES(269734834,8);
INSERT INTO certified VALUES(269734834,9);
INSERT INTO certified VALUES(269734834,10);
INSERT INTO certified VALUES(269734834,11);
INSERT INTO certified VALUES(269734834,12);
INSERT INTO certified VALUES(269734834,13);
INSERT INTO certified VALUES(269734834,14);
INSERT INTO certified VALUES(269734834,15);
INSERT INTO certified VALUES(552455318,7);
INSERT INTO certified VALUES(556784565,5);
INSERT INTO certified VALUES(556784565,2);
INSERT INTO certified VALUES(556784565,3);
INSERT INTO certified VALUES(573284895,3);
INSERT INTO certified VALUES(573284895,4);
INSERT INTO certified VALUES(573284895,5);
INSERT INTO certified VALUES(574489456,8);
INSERT INTO certified VALUES(574489456,6);
INSERT INTO certified VALUES(574489457,7);
INSERT INTO certified VALUES(242518965,2);
INSERT INTO certified VALUES(242518965,10);
INSERT INTO certified VALUES(141582651,2);
INSERT INTO certified VALUES(141582651,10);
INSERT INTO certified VALUES(141582651,12);
INSERT INTO certified VALUES(011564812,2);
INSERT INTO certified VALUES(011564812,10);
INSERT INTO certified VALUES(356187925,6);
INSERT INTO certified VALUES(159542516,5);
INSERT INTO certified VALUES(159542516,7);
INSERT INTO certified VALUES(090873519,6);
--suppliers
INSERT INTO suppliers VALUES(1,'Acme Widget Suppliers','1 Grub St., Potemkin Village, IL 61801');
INSERT INTO suppliers VALUES(2,'Big Red Tool and Die','4 My Way, Bermuda Shorts, OR 90305');
INSERT INTO suppliers VALUES(3,'Perfunctory Parts','99999 Short Pier, Terra Del Fuego, TX 41299');
INSERT INTO suppliers VALUES(4,'Alien Aircaft Inc.','2 Groom Lake, Rachel, NV 51902');
--parts
INSERT INTO parts VALUES(1,'Left Handed Bacon Stretcher Cover','Red');
INSERT INTO parts VALUES(2,'Smoke Shifter End','Black');
INSERT INTO parts VALUES(3,'Acme Widget Washer','Red');
INSERT INTO parts VALUES(4,'Acme Widget Washer','Silver');
INSERT INTO parts VALUES(5,'I Brake for Crop Circles Sticker','Translucent');
INSERT INTO parts VALUES(6,'Anti-Gravity Turbine Generator','Cyan');
INSERT INTO parts VALUES(7,'Anti-Gravity Turbine Generator','Magenta');
INSERT INTO parts VALUES(8,'Fire Hydrant Cap','Red');
INSERT INTO parts VALUES(9,'7 Segment Display','Green');
--catalog
INSERT INTO catalog VALUES(1,3,0.50);
INSERT INTO catalog VALUES(1,4,0.50);
INSERT INTO catalog VALUES(1,8,11.70);
INSERT INTO catalog VALUES(2,3,0.55);
INSERT INTO catalog VALUES(2,8,7.95);
INSERT INTO catalog VALUES(2,1,16.50);
INSERT INTO catalog VALUES(3,8,12.50);
INSERT INTO catalog VALUES(3,9,1.00);
INSERT INTO catalog VALUES(4,5,2.20);
INSERT INTO catalog VALUES(4,6,1247548.23);
INSERT INTO catalog VALUES(4,7,1247548.23);
--sailors
INSERT INTO sailors VALUES(18,'jones',3,30.0);
INSERT INTO sailors VALUES(41,'jonah',6,56.0);
INSERT INTO sailors VALUES(22,'ahab',7,44.0);
INSERT INTO sailors VALUES(63,'moby',NULL,15.0);
