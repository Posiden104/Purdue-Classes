CREATE TABLE University(
	UnivId 	INTEGER CHECK (UnivId > 0 AND UnivId <= 9999),
	UnivName	VARCHAR2(40) NOT NULL,
	PRIMARY KEY (UnivId));
	
CREATE TABLE Department(
	DeptId 	INTEGER CHECK (DeptId > 0),
	DeptName	VARCHAR2(40) NOT NULL,
	PRIMARY KEY (DeptId));

CREATE TABLE Employee(
	EmpId 		INTEGER CHECK (EmpId > 0),
	EmpName		VARCHAR2(50) NOT NULL,
	DeptId INTEGER,
	ZipCode	INTEGER CHECK (ZipCode > 0),
	PRIMARY KEY (EmpId),
	FOREIGN KEY (DeptId) REFERENCES Department (DeptId));

CREATE TABLE Project(
	ProjId 	INTEGER CHECK (ProjId > 0),
	ProjName	VARCHAR2(40) NOT NULL,
	PRIMARY KEY (ProjId));
/*
we assume that an employee is graduated from only one university
*/
CREATE TABLE Graduate(
	EmpId 	INTEGER CHECK (EmpId > 0),
	UnivId	INTEGER NOT NULL,
	GradYear INTEGER CHECK (GradYear > 1950),
	PRIMARY KEY (EmpId),
	FOREIGN KEY (EmpId) REFERENCES Employee (EmpId),
	FOREIGN KEY (UnivId) REFERENCES University (UnivId));

CREATE TABLE EmpProject(
	EmpId 	INTEGER NOT NULL,
	ProjId	INTEGER NOT NULL,
	StartDate DATE Not Null,
	EndDate DATE, /*null means current*/
	PRIMARY KEY (EmpId, ProjId, StartDate),
	FOREIGN KEY (EmpId) REFERENCES Employee (EmpId),
	FOREIGN KEY (ProjId) REFERENCES Project (ProjId));


CREATE TABLE ProjectManager(
	ProjId 	INTEGER NOT NULL,
	MgrId	INTEGER NOT NULL,
	StartDate DATE Not Null,
	EndDate DATE, /*null means current*/
	PRIMARY KEY (ProjId, MgrId, StartDate),
	FOREIGN KEY (MgrId) REFERENCES Employee (EmpId),
	FOREIGN KEY (ProjId) REFERENCES Project (ProjId));

-- following lines are sample dataset.
-- you will need to populate more ones to see/test each query.

INSERT INTO University VALUES (1, 'Purdue');
INSERT INTO University VALUES (2, 'Maryland');
INSERT INTO University VALUES (3, 'UCSB');

INSERT INTO Department VALUES (1, 'Development');
INSERT INTO Department VALUES (2, 'UX Design');
INSERT INTO Department VALUES (3, 'Technical Architects');


INSERT INTO Employee VALUES (1, 'James', 1, 47907);
INSERT INTO Employee VALUES (2, 'Michael', 1, 47905);
INSERT INTO Employee VALUES (3, 'Lin', 2, 47900);
INSERT INTO Employee VALUES (4, 'Rafel', 3, 47906);
INSERT INTO Employee VALUES (5, 'Ben', 3, 47906);
INSERT INTO Employee VALUES (6, 'Bert', 2, 47905);

INSERT INTO Project VALUES (1, 'Mobile');
INSERT INTO Project VALUES (2, 'UTC');
INSERT INTO Project VALUES (3, 'CD Finance');

INSERT INTO Graduate VALUES (1, 1, 1995);
INSERT INTO Graduate VALUES (2, 1, 2012);
INSERT INTO Graduate VALUES (3, 2, 1999);
INSERT INTO Graduate VALUES (4, 3, 2001);
INSERT INTO Graduate VALUES (5, 1, 2005);
INSERT INTO Graduate VALUES (6, 2, 2012);

INSERT INTO EmpProject VALUES(1, 1, '20-DEC-14', '20-FEB-15');
INSERT INTO EmpProject VALUES(4, 1, '30-MAY-14', '10-SEP-15');
INSERT INTO EmpProject VALUES(2, 1, '30-MAY-12', '10-SEP-14');
INSERT INTO EmpProject VALUES(3, 2, '30-MAY-12', NULL);

INSERT INTO ProjectManager VALUES(1, 1, '20-DEC-14', '20-FEB-15');
INSERT INTO ProjectManager VALUES(1, 2, '20-FEB-15', NULL);
INSERT INTO ProjectManager VALUES(2, 3, '30-MAY-14', '20-FEB-16');
INSERT INTO ProjectManager VALUES(3, 2, '30-MAY-12', NULL);
