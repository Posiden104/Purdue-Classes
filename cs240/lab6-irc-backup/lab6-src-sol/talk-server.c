
const char * usage =
"                                                               \n"
"talk-server:                                                \n"
"                                                               \n"
"Simple server program used to communicate multiple users       \n"
"                                                               \n"
"To use it in one window type:                                  \n"
"                                                               \n"
"   talk-server <port>                                       \n"
"                                                               \n"
"Where 1024 < port < 65536.             \n"
"                                                               \n"
"In another window type:                                       \n"
"                                                               \n"
"   telnet <host> <port>                                        \n"
"                                                               \n"
"where <host> is the name of the machine where talk-server  \n"
"is running. <port> is the port number you used when you run   \n"
"daytime-server.                                               \n"
"                                                               \n";

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include "linked_list.h"

#define PASSWORD_FILE "password.txt"
LINKED_LIST * users;
LINKED_LIST * usersInRoom;

typedef struct MESSAGE {
	char * user;
	char * message;
	int messageNum;
} MESSAGE;

// List of messages. They wrap around
#define MAX_MESSAGES 100
int messageNum=1;
int lastMessage;
MESSAGE messages[MAX_MESSAGES];

int QueueLength = 5;

// Processes time request
void initialize();
void processRequest( int socket );
void addUser(int fd, char * user, char * password, char * args);
void enterRoom(int fd, char * user, char * password, char * args);
void leaveRoom(int fd, char * user, char * password, char * args);
void sendMessage(int fd, char * user, char * password, char * args);
void getMessages(int fd, char * user, char * password, char * args);
void getUsersInRoom(int fd, char * user, char * password, char * args);
void getAllUsers(int fd, char * user, char * password, char * args);
void addMessage(char * user, char * message);

int open_server_socket(int port) {

	// Set the IP address and port for this server
	struct sockaddr_in serverIPAddress; 
	memset( &serverIPAddress, 0, sizeof(serverIPAddress) );
	serverIPAddress.sin_family = AF_INET;
	serverIPAddress.sin_addr.s_addr = INADDR_ANY;
	serverIPAddress.sin_port = htons((u_short) port);
  
	// Allocate a socket
	int masterSocket =  socket(PF_INET, SOCK_STREAM, 0);
	if ( masterSocket < 0) {
		perror("socket");
		exit( -1 );
	}

	// Set socket options to reuse port. Otherwise we will
	// have to wait about 2 minutes before reusing the sae port number
	int optval = 1; 
	int err = setsockopt(masterSocket, SOL_SOCKET, SO_REUSEADDR, 
			     (char *) &optval, sizeof( int ) );
	
	// Bind the socket to the IP address and port
	int error = bind( masterSocket,
			  (struct sockaddr *)&serverIPAddress,
			  sizeof(serverIPAddress) );
	if ( error ) {
		perror("bind");
		exit( -1 );
	}
	
	// Put socket in listening mode and set the 
	// size of the queue of unprocessed connections
	error = listen( masterSocket, QueueLength);
	if ( error ) {
		perror("listen");
		exit( -1 );
	}

	return masterSocket;
}

int
main( int argc, char ** argv )
{
	// Print usage if not enough arguments
	if ( argc < 2 ) {
		fprintf( stderr, "%s", usage );
		exit( -1 );
	}
	
	// Get the port from the arguments
	int port = atoi( argv[1] );
	
	int masterSocket = open_server_socket(port);

	initialize();
	
	while ( 1 ) {
		
		// Accept incoming connections
		struct sockaddr_in clientIPAddress;
		int alen = sizeof( clientIPAddress );
		int slaveSocket = accept( masterSocket,
					  (struct sockaddr *)&clientIPAddress,
					  (socklen_t*)&alen);
		
		if ( slaveSocket < 0 ) {
			perror( "accept" );
			exit( -1 );
		}
		
		// Process request.
		processRequest( slaveSocket );		
	}
}

//
// Commands:
//   Commands are started y the client.
//
//   Request: ADD-USER <USER> <PASSWD>\r\n
//   Answer: OK\r\n or DENIED\r\n
//
//    REQUEST: GET-ALL-USERS <USER> <PASSWD>\r\n
//    Answer: USER1\r\n
//            USER2\r\n
//            ...
//            \r\n
//
//   Request: ENTER-ROOM <USER> <PASSWD>\r\n
//   Answer: OK\n or DENIED\r\n
//
//   Request: LEAVE-ROOM <USER> <PASSWD>\r\n
//   Answer: OK\n or DENIED\r\n
//
//   Request: SEND-MESSAGE <USER> <PASSWD> <MESSAGE>\n
//   Answer: OK\n or DENIED\n
//
//   Request: GET-MESSAGES <USER> <PASSWD> <LAST-MESSAGE-NUM>\r\n
//   Answer: MSGNUM1 USER1 MESSAGE1\r\n
//           MSGNUM2 USER2 MESSAGE2\r\n
//           MSGNUM3 USER2 MESSAGE2\r\n
//           ...\r\n
//           \r\n
//
//    REQUEST: GET-USERS-IN-ROOM <USER> <PASSWD>\r\n
//    Answer: USER1\r\n
//            USER2\r\n
//            ...
//            \r\n
//

void
processRequest( int fd )
{
	// Buffer used to store the comand received from the client
	const int MaxCommandLine = 1024;
	char commandLine[ MaxCommandLine + 1 ];
	int commandLineLength = 0;
	int n;
	
	// Currently character read
	unsigned char prevChar = 0;
	unsigned char newChar = 0;
	
	//
	// The client should send COMMAND-LINE\n
	// Read the name of the client character by character until a
	// \n is found.
	//

	// Read character by character until a \n is found or the command string is full.
	while ( commandLineLength < MaxCommandLine &&
		read( fd, &newChar, 1) > 0 ) {

		if (newChar == '\n' && prevChar == '\r') {
			break;
		}
		
		commandLine[ commandLineLength ] = newChar;
		commandLineLength++;

		prevChar = newChar;
	}
	
	// Add null character at the end of the string
	// Eliminate last \r
	commandLineLength--;
        commandLine[ commandLineLength ] = 0;

	// Get command
	char * command = commandLine;
	char * space = strchr(command, ' ');
	if (space==NULL) {
		// No space. Send denied
		const char * msg =  "ERROR (no command)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
		return;
	}

	// Skip space
	*space = 0;
	space++;

	printf( "command=%s\n", command );

	// Find user
	char * user = space;
	space = strchr(space, ' ');
	if (space==NULL) {
		// No space. Send denied
		const char * msg =  "ERROR (no user)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
		return;
	}
	
	// Skip space
	*space = 0;
	space++;

	printf( "user=%s\n", user );

	// Find password
	char * password = space;
	space = strchr(password, ' ');

	if (space != NULL) {
		*space = 0;
		space++;
	}
	else {
		// Skip to end of string
		space = password + strlen(password);
	}
	
	printf( "password=%s\n", password );

	char * args = space;
	printf( "args=%s\n", args );

	if (!strcmp(command, "ADD-USER")) {
		addUser(fd, user, password, args);
	}
	else if (!strcmp(command, "ENTER-ROOM")) {
		enterRoom(fd, user, password, args);
	}
	else if (!strcmp(command, "LEAVE-ROOM")) {
		leaveRoom(fd, user, password, args);
	}
	else if (!strcmp(command, "SEND-MESSAGE")) {
		sendMessage(fd, user, password, args);
	}
	else if (!strcmp(command, "GET-MESSAGES")) {
		getMessages(fd, user, password, args);
	}
	else if (!strcmp(command, "GET-USERS-IN-ROOM")) {
		getUsersInRoom(fd, user, password, args);
	}
	else if (!strcmp(command, "GET-ALL-USERS")) {
		getAllUsers(fd, user, password, args);
	}
	else {
		const char * msg =  "UNKNOWN COMMAND\n";
		write(fd, msg, strlen(msg));
	}

	// Send OK answer
	//const char * msg =  "OK\n";
	//write(fd, msg, strlen(msg));


	close(fd);
	
#if 0
	// Get time of day
	time_t now;
	time(&now);
	char	*timeString = ctime(&now);
	
	// Send name and greetings
	const char * hi = "\nHi ";
	const char * timeIs = " the time is:\n";
	write( fd, hi, strlen( hi ) );
	write( fd, name, strlen( name ) );
	write( fd, timeIs, strlen( timeIs ) );
	
	// Send the time of day 
	write(fd, timeString, strlen(timeString));
	
	// Send last newline
	const char * newline="\n";
	write(fd, newline, strlen(newline));
#endif
	
}

void initialize()
{
	// Open password file
	users = llist_create();
	int result = llist_read(users, PASSWORD_FILE);
	if ( result == 0 ) {
		// No password file. Create it.
		result = llist_save(users, PASSWORD_FILE);
		if (result==0) {
			printf("Cannot open %s\n", PASSWORD_FILE);
			exit(1);
		}
	}

	// Initialize users in room
	usersInRoom = llist_create();
	if ( usersInRoom == NULL ) {
		if (result==0) {
			printf("Cannot create users in room\n");
			exit(1);
		}
	}

	// Message list is a global variable so we do not need to initialize it

}

int checkPassword(int fd, char * user, char * password) {
	// Check password
	char * user_password = llist_lookup(users, user);
	if (user_password == NULL || strcmp(user_password, password)) {
		const char * msg =  "ERROR (Wrong password)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
		return 0;		
	}

	return 1;
	
}

void addUser(int fd, char * user, char * password, char * args)
{
	if (llist_lookup(users, user)!=NULL) {
		const char * msg =  "ERROR (User Exists)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
		return;		
	}

	llist_add(users, user, password);
	llist_save(users, PASSWORD_FILE);

	const char * msg =  "OK\r\n";
	write(fd, msg, strlen(msg));
	close(fd);

	return;		
}

void enterRoom(int fd, char * user, char * password, char * args)
{
	if (!checkPassword(fd, user, password)) {
		return;
	}
	
	llist_add(usersInRoom, user, "");

	addMessage(user, " entered the room.");

	const char * msg =  "OK\r\n";
	write(fd, msg, strlen(msg));
	close(fd);

}

void leaveRoom(int fd, char * user, char * password, char * args)
{
	if (!checkPassword(fd, user, password)) {
		return;
	}
	
	if (llist_lookup(usersInRoom, user)==NULL) {
		const char * msg =  "DENIED (not in room)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
	}
	
	llist_remove(usersInRoom, user);

	addMessage(user, " left the room");

	const char * msg =  "OK\r\n";
	write(fd, msg, strlen(msg));
	close(fd);
}

void addMessage(char * user, char * message) {
	if (messages[lastMessage].user=NULL) {
		free(messages[lastMessage].user);
		free(messages[lastMessage].message);
	}
	
	messages[lastMessage].user = strdup(user);
	messages[lastMessage].message = strdup(message);
	messages[lastMessage].messageNum = messageNum;
	lastMessage = ((lastMessage +1) % MAX_MESSAGES);
	messageNum++;
}

void sendMessage(int fd, char * user, char * password, char * args)
{
	if (!checkPassword(fd, user, password)) {
		return;
	}

	if (llist_lookup(usersInRoom, user)==NULL) {
		const char * msg =  "DENIED (not in room)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
	}
	
	char * message = args;

	addMessage(user, message);

	const char * msg =  "OK\r\n";
	write(fd, msg, strlen(msg));
	close(fd);
}

void getMessages(int fd, char * user, char * password, char * args)
{
	if (!checkPassword(fd, user, password)) {
		return;
	}

	if (llist_lookup(usersInRoom, user)==NULL) {
		const char * msg =  "DENIED (not in room)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
	}
	
	int messageNumFrom;
	sscanf(args, "%d", &messageNumFrom);

	int i;
	int j = lastMessage;
	int k = 0;
	for ( i=0; i<MAX_MESSAGES; i++) {
		if (messages[j].user != NULL && messages[j].messageNum > messageNumFrom) {
			char msgNum[20];
			sprintf(msgNum, "%d", messages[j].messageNum);
			write(fd, msgNum, strlen(msgNum));
			write(fd, " ", 1 );
			write(fd, messages[j].user, strlen(messages[j].user));
			write(fd, " ", 1 );
			write(fd, messages[j].message, strlen(messages[j].message));
			write(fd, "\r\n", 2 );
			k++;
		}
		j = (j+1)%MAX_MESSAGES;
	}

	if (k==0) {
		// No new messages
		const char * msg =  "NO-NEW-MESSAGES\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
		return;
	}
	write(fd, "\r\n", 2);
	
	close(fd);
}

void getUsersInRoom(int fd, char * user, char * password, char * args)
{
	if (!checkPassword(fd, user, password)) {
		return;
	}
	
	if (llist_lookup(usersInRoom, user)==NULL) {
		const char * msg =  "DENIED (not in room)\r\n";
		write(fd, msg, strlen(msg));
		close(fd);
	}
	
	int nusers = llist_number_elements(usersInRoom);
	int i;
	for (i = 0; i < nusers; i++) {
		char * name;
		char * pass;
		llist_get_ith(usersInRoom, i, &name, &pass);
		write(fd, name, strlen(name));
		write(fd, "\r\n", 2);
	}
	write(fd, "\r\n", 2);
	close(fd);
}

void getAllUsers(int fd, char * user, char * password, char * args)
{
	if (!checkPassword(fd, user, password)) {
		return;
	}
	
	int nusers = llist_number_elements(users);
	int i;
	for (i = 0; i < nusers; i++) {
		char * name;
		char * pass;
		llist_get_ith(users, i, &name, &pass);
		write(fd, name, strlen(name));
		write(fd, "\r\n", 2);
	}
	write(fd, "\r\n", 2);
	close(fd);
}

