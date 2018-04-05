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

echo Enter room
./test-talk-server localhost $PORT "ENTER-ROOM superman clarkkent"
./test-talk-server localhost $PORT "ENTER-ROOM aquaman xyz"

echo Print users in room
./test-talk-server localhost $PORT "GET-USERS-IN-ROOM superman clarkkent"

echo Enter another user
./test-talk-server localhost $PORT "ENTER-ROOM mary poppins"

echo Print users in room
./test-talk-server localhost $PORT "GET-USERS-IN-ROOM mary poppins"

# Kill server
echo Killing Server
PID=`/usr/bin/ps | grep talk-ser | awk '{ print $1;}'`
kill -9 $PID
