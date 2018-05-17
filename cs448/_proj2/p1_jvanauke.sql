-- Joel Van Auken
-- jvanauke


-- Question 1

select EmpName from Employee
where ZipCode = '47906'
OR ZipCode = '47907';

-- Question 2

select ProjName 
from ProjectManager join Project
on Project.ProjId = ProjectManager.ProjId
where EndDate is NULL;


-- Question 3

select ProjName from Project
order by ProjName desc;

-- Question 4

select u.UnivName, COUNT(g.EmpId) 
from University u 
join Graduate g
on u.UnivId = g.UnivId
group by u.univName;

-- Question 5

select e.EmpName, d.DeptName, g.GradYear
from Employee e
join Department d on e.DeptId = d.DeptId
join Graduate g on e.EmpId = g.EmpId;

-- Question 6

select EmpName
from Employee
where DeptId = '1'
order by EmpName asc;

-- Question 7

select e.EmpName
from Employee e
join Graduate g on e.EmpId = g.EmpId
join University u on u.UnivId = g.UnivId
where u.UnivName = 'Purdue'
and g.GradYear <= '2005';

-- Question 8

select ZipCode, count(EmpId)
from Employee
group by ZipCode
order by ZipCode desc;

-- Question 9

select e.EmpName
from Employee e
join Graduate g on e.EmpId = g.EmpId
where g.GradYear = (
	select max(GradYear)
	from Graduate);

-- Question 10

select p.ProjName, e.EmpName
from Project p
join EmpProject ep on p.ProjId = ep.ProjId
join Employee e on ep.EmpId = e.EmpId
order by p.ProjName, e.empName asc;

