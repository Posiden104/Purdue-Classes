-- 1.1 Find the names of movies that are currently running at house-full

SELECT DISTINCT title
FROM Movies, Tickets, Showtimes 
WHERE Showtimes.movieID = Movies.movieID
AND Tickets.showID = Showtimes.showID 
AND (SELECT COUNT(showID) FROM Tickets) >= Showtimes.max_occupancy;

-- 1.2 Display the names of theaters screening 3 or more shows; also show the number of shows each such theater is screening.

ELECT Theaters.name, COUNT(Showtimes.theaterID) as numShows
FROM Showtimes
RIGHT JOIN Theaters ON Showtimes.theaterID = Theaters.theaterID
WHERE (SELECT COUNT(Showtimes.theaterID) FROM Showtimes WHERE Showtimes.theaterID = Theaters.theaterID) >= 3
GROUP BY Theaters.name;

-- 1.3 Find the name and id of the theaters which have at-least one show for every movie.

SELECT DISTINCT Theaters.theaterID, Theaters.name
FROM Movies, Showtimes, Theaters
WHERE Theaters.theaterID = Showtimes.theaterID 
AND (SELECT COUNT(theaterID) FROM Showtimes WHERE Theaters.theaterID = Showtimes.theaterID) >= (SELECT COUNT(movieID) FROM Movies);

-- 1.4 Find the email ids and age groups of users who have booked tickets for only one show currently 
-- Also, for each such user, display the name of the movie they have booked their tickets for
SELECT email, Users.userID, age, Movies.title
FROM Users, Tickets, Movies, Showtimes
WHERE Users.userID = Tickets.userID
AND Tickets.showID = Showtimes.showID
AND Showtimes.movieID = Movies.movieID
AND (SELECT COUNT(userID) FROM Tickets WHERE Users.userID = Tickets.userID) = 1;

-- 1.5 Find the names of the movies where there exists at least one show that only has users belonging to one age group
SELECT DISTINCT Movies.title
FROM (SELECT COUNT(DISTINCT age) as numAges, Tickets.showID as showNum
FROM Showtimes, Tickets
FULL JOIN Users ON Users.userID = Tickets.userID
WHERE Showtimes.showID = Tickets.showID
GROUP BY Tickets.showID), Showtimes
FULL JOIN Movies ON Showtimes.movieID = Movies.movieID
WHERE numAges = 1
AND Showtimes.showID = showNum
AND Showtimes.movieID = Movies.movieID;

-- 1.6 Find the name(s) of the theater(s) that have a show with the largest number of bookings by users. If there are ties, display all theaters that qualify
SELECT name
FROM(SELECT COUNT(Tickets.showID) as numTickets, Tickets.showID as showNum
FROM Tickets
GROUP BY Tickets.showID), Showtimes, Theaters
WHERE numTickets = (SELECT MAX(numTickets) FROM (SELECT COUNT(Tickets.showID) as numTickets, Tickets.showID as showNum
FROM Tickets
GROUP BY Tickets.showID))
AND showNum = Showtimes.showID
AND Showtimes.theaterID = Theaters.theaterID;

-- 1.7 For each user, display their name, email id and the theater in which he/she has watched most shows

-- 1.8

-- 1.9 For every show, find the number of seats left in the show
SELECT DISTINCT showNum, Showtime.max_occupancy - numTickets as num_Tickets_Left
FROM (SELECT COUNT(Tickets.showID) as numTickets, Tickets.showID as showNum
FROM Tickets, Users
WHERE Users.userID = Tickets.userID
GROUP BY Tickets.showID), Showtimes
WHERE showNum = Showtimes.showID
ORDER BY showNum;
