#/bin/bash

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` server-prog port"
  exit 1
fi

SERVER=$1
PORT=$2

#Start server in the background
rm password.txt
$SERVER $PORT > talk-server.out &
sleep 1

echo Add Users
./test-talk-server localhost $PORT "ADD-USER superman clarkkent"
./test-talk-server localhost $PORT "ADD-USER spiderman peterpark"
./test-talk-server localhost $PORT "ADD-USER aquaman xyz"
./test-talk-server localhost $PORT "ADD-USER mary poppins"

./test-talk-server localhost $PORT "GET-ALL-USERS superman badpassword"

echo Test password in GET_ALL_USERS
./test-talk-server localhost $PORT "GET-ALL-USERS superman badpassword"
./test-talk-server localhost $PORT "GET-ALL-USERS baduser badpassword"

echo Test password in ENTER-ROOM
./test-talk-server localhost $PORT "ENTER-ROOM superman badpassword"
./test-talk-server localhost $PORT "ENTER-ROOM baduser badpassword"

echo Test password in GET-USERS-IN-ROOM
./test-talk-server localhost $PORT "GET-USERS-IN-ROOM superman badpassword"
./test-talk-server localhost $PORT "GET-USERS-IN-ROOM baduser badpassword"

echo Test password in LEAVE-ROO
./test-talk-server localhost $PORT "LEAVE-ROOM superman badpassword"
./test-talk-server localhost $PORT "LEAVE-ROOM baduser badpassword"

echo Test password in SEND-MESSAGE
./test-talk-server localhost $PORT "SEND-MESSAGE superman badpassword"
./test-talk-server localhost $PORT "SEND-MESSAGE baduser badpassword"

echo Test password in GET-MESSAGES
./test-talk-server localhost $PORT "GET-MESSAGES superman badpassword"
./test-talk-server localhost $PORT "GET-MESSAGES baduser badpassword"

echo Test password OK
./test-talk-server localhost $PORT "GET-ALL-USERS mary poppins"

# Kill server
echo Killing Server
PID=`/usr/bin/ps | grep talk-ser | awk '{ print $1;}'`
kill -9 $PID
