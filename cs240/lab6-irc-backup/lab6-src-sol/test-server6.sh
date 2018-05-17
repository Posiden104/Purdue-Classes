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

echo Send message
./test-talk-server localhost $PORT "SEND-MESSAGE mary poppins Hi everybody!"
./test-talk-server localhost $PORT "SEND-MESSAGE mary poppins Welcome to the talk program!"

echo Get messages
./test-talk-server localhost $PORT "GET-MESSAGES superman clarkkent 0"

echo Send message
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent Hi Mary!"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent Here I am working on cs240"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent I am testing the project"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 1"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 2"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 3"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 4"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 5"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 6"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 7"
./test-talk-server localhost $PORT "SEND-MESSAGE superman clarkkent message 8"

echo Get messages from 0
./test-talk-server localhost $PORT "GET-MESSAGES mary poppins 0"

echo Enter another user
./test-talk-server localhost $PORT "LEAVE-ROOM mary poppins"

echo Get messages from 2
./test-talk-server localhost $PORT "GET-MESSAGES superman clarkkent 2"

# Kill server
echo Killing Server
PID=`/usr/bin/ps | grep talk-ser | awk '{ print $1;}'`
kill -9 $PID

