
#include <time.h>
#include <ncurses/curses.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

char * user;
char * password;

int current_getch;
int doloop = 1;
static WINDOW *mainwnd;
static WINDOW *screen;
WINDOW *my_win;

int now_sec, now_min, now_hour, now_day, now_wday, now_month, now_year;
time_t now;
struct tm *now_tm;

char * host;
char * sport;
char * user;
char * password;
int port;

#define MAX_MESSAGES 50
#define MAX_MESSAGE_LEN 200
int nmessages;
char messages[MAX_MESSAGES][MAX_MESSAGE_LEN];

int open_client_socket(char * host, int port) {
	// Initialize socket address structure
	struct  sockaddr_in socketAddress;
	
	// Clear sockaddr structure
	memset((char *)&socketAddress,0,sizeof(socketAddress));
	
	// Set family to Internet 
	socketAddress.sin_family = AF_INET;
	
	// Set port
	socketAddress.sin_port = htons((u_short)port);
	
	// Get host table entry for this host
	struct  hostent  *ptrh = gethostbyname(host);
	if ( ptrh == NULL ) {
		perror("gethostbyname");
		exit(1);
	}
	
	// Copy the host ip address to socket address structure
	memcpy(&socketAddress.sin_addr, ptrh->h_addr, ptrh->h_length);
	
	// Get TCP transport protocol entry
	struct  protoent *ptrp = getprotobyname("tcp");
	if ( ptrp == NULL ) {
		perror("getprotobyname");
		exit(1);
	}
	
	// Create a tcp socket
	int sock = socket(PF_INET, SOCK_STREAM, ptrp->p_proto);
	if (sock < 0) {
		perror("socket");
		exit(1);
	}
	
	// Connect the socket to the specified server
	if (connect(sock, (struct sockaddr *)&socketAddress,
		    sizeof(socketAddress)) < 0) {
		perror("connect");
		exit(1);
	}
	
	return sock;
}

#define MAX_RESPONSE (10 * 1024)
int sendCommand(char * host, int port, char * command, char * user,
		char * password, char * args, char * response) {
	int sock = open_client_socket( host, port);

	// Send command
	write(sock, command, strlen(command));
	write(sock, " ", 1);
	write(sock, user, strlen(user));
	write(sock, " ", 1);
	write(sock, password, strlen(password));
	write(sock, " ", 1);
	write(sock, args, strlen(args));
	write(sock, "\r\n",2);

	// Keep reading until connection is closed or MAX_REPONSE
	int n = 0;
	int len = 0;
	while ((n=read(sock, response+len, MAX_RESPONSE - len))>0) {
		len += n;
	}

	printf("response:%s\n", response);

	close(sock);
}
	
void screenInit(void) {
	// Init curses
	mainwnd = initscr();

	// Characters ar enot echoed when typed. 
	noecho();

	// Raw input
	cbreak();

	// Make characters available immediately
	nodelay(mainwnd, TRUE);

	// Refresh screen
	refresh();
	wrefresh(mainwnd);

	// Create window 24 rows by 80 columns
	screen = newwin(23, 78, 1, 1);
	box(screen, ACS_VLINE, ACS_HLINE);
}

static void update_display(void) {
#if 0
	curs_set(0);
	mvwprintw(screen,1,1,"-------- HEADER --------");
	mvwprintw(screen,3,6,"TIME: %d:%d:%d", now_hour, now_min, now_sec);
	mvwprintw(screen,5,6,"DATE: %d-%d-%d", now_day, now_month, now_year);
	mvwprintw(screen,7,6,"PRESS q TO END");
	mvwprintw(screen,10,1,"-------- FOOTER --------");
	wrefresh(screen);
	refresh();
#endif
	curs_set(0);

	//mvwprintw(screen,1,1,"            TALK %s:%d ", host, port);

	getMessages();

	// Print messages
	int i = 0;
	for (i=0; i < 20; i++) {
		mvwprintw(screen,23-i,1, messages[i] );		
	}

	wrefresh(screen);
	refresh();
}

void screen_end(void) {
	endwin();
}

void maketime(void) {
	// Get the current date/time
	now = time (NULL);
	now_tm = localtime (&now);
	now_sec = now_tm->tm_sec;
	now_min = now_tm->tm_min;
	now_hour = now_tm->tm_hour;
	now_day = now_tm->tm_mday;
	now_wday = now_tm->tm_wday;
	now_month = now_tm->tm_mon + 1;
	now_year = now_tm->tm_year + 1900;
}

void printUsage()
{
	printf("Usage: talk-client host port user password\n");
	exit(1);
}

void add_user() {
	// Try first to add user in case it does not exist.
	char response[ MAX_RESPONSE ];
	sendCommand(host, port, "ADD-USER", user, password, "Me", response);
	
	if (!strcmp(response,"OK\r\n")) {
		printf("User %s added\n", user);
	}
}

void get_messages() {
	// Try first to add user in case it does not exist.
	char response[ MAX_RESPONSE ];
	sendCommand(host, port, "GET-MESSAGES", user, password, "0", response);

	// Separate messages
	char * p = response;
	int i = 0;
	int j = 0;
	while (*p) {
		if (*p == '\r') {
			// Skip /n
			p++;
		}
		else {
			messages[i][j] = *p;
			p++;
		}
	}
	if (!strcmp(response,"OK\r\n")) {
		printf("User %s added\n", user);
	}
}


int main(int argc, char **argv) {

	if (argc < 5) {
		printUsage();
	}

	host = argv[1];
	sport = argv[2];
	user = argv[3];
	password = argv[4];

	sscanf(sport, "%d", &port);

	add_user();
	
	screenInit();
	while (doloop) {
		current_getch = getch();
		if (current_getch == 113) {
			doloop = 0;
		}
		//display_new_messages();
		update_display();
		usleep(100000);
	}
	screen_end();
	printf("TEST ENDS\n");
	return 0;
}



