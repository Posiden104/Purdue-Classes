
#include<stdio.h>
#include<string.h>
#include<stdlib.h>

typedef struct date{
	int month;
	int day;
	int year;
} date;

typedef struct person{ 
	int num;
	char title[5];
	char fname[255];
	char mname[255];
	char lname[255];
	char asso[255];
	struct date* date;
	struct person* next;
} person;

int enterInfo();
void printInfo();

char * menu = "# Menu\n1. Enter Attendee Information\n2. List All Attendees\n3. Quit\n\nWhat do you want to do? (1/2/3) ";
person* head = NULL;
int global_num = 1;

int main(){
	char input[128], selection[1];
	memset(input, '\0', 128);
	memset(selection, '\0', 1);

	while(selection[0] != '3'){
		printf(menu);
		fgets(input, 128, stdin);
		if(input[0] == '3') {
			break;
		}
	
		sscanf(input, "%1[1-3]", selection);

		if(selection[0] == '1'){
			printf("\n## Attendees Added: %d\n\n\n", enterInfo());
		}
		if(selection[0] == '2'){
			printInfo();
		}
	
	}

	printf("\nGood bye!\n");
	return 0;
}

int enterInfo(){
	/* create input fields*/
	char input[255];
	person* p;
	person* p2;
	date* d;
	int added = 0;
	p2 = head;
	if(p2 != NULL){
		while(p2->next != NULL){
			p2 = p2->next;
		}
	}
	
	printf("\n# Enter Attendee Information\n");

	while(1){
		memset(input, '\0', 255);
	
		/* create person node*/
		p = malloc(sizeof(person));
		if(head == NULL) head = p;

		d = malloc(sizeof(date));
	
		/* fill in fields*/
		printf("Title: ");
		fgets(input, 255, stdin);
		if(input[0] == '\n'){
			/* User done entering information*/
			return added;
		}
		sscanf(input, "%5[a-zA-Z0-9 .]", p->title);
		

		printf("First Name: ");
		fgets(input, 255, stdin);
		sscanf(input, "%255[a-zA-Z0-9 ]", p->fname);
	

		printf("Middle Name: ");
		fgets(input, 255, stdin);
		sscanf(input, "%255[a-zA-Z0-9 ]", p->mname);
		

		printf("Last Name: ");
		fgets(input, 255, stdin);
		sscanf(input, "%255[a-zA-Z0-9 ]", p->lname);
		

		printf("Association: ");
		fgets(input, 255, stdin);
		sscanf(input, "%31[a-zA-Z0-9 ]", p->asso);
		

		printf("Member Since(MM/DD/YYYY): ");
		fgets(input, 255, stdin);
		sscanf(input, "%d/%d/%d", &d->month, &d->day, &d->year);
		
		p->date = d;
		p->next = NULL;

		p->num = global_num;
		global_num += 1;
		added += 1;
		printf("\n");

		/* insert person into linked list */
		if(p2 != NULL){
			p2->next = p;
		}

		p2 = p;
	}	

	return -1;
}
void printInfo(){
	person* p;
	p = head;
	if(head == NULL){
		printf("\nYou have not entered any attendees yet. Please enter some attendees\n\n\n");
		return;
	}
	
	printf("\n\n# List of All attendees\n");
	printf("## There are %d attendees registered.\n\n", global_num -1);


	while(p != NULL){
		printf("%d. ", p->num);
		if(p->title != NULL) printf("%s ", p->title);
		if(p->fname != NULL) printf("%s ", p->fname);
		if(*p->mname != '\0') printf("%s ", p->mname);
		if(p->lname != NULL) printf("%s ", p->lname);
		if(p->asso != NULL) printf("(%s) ", p->asso);
		if(p->date != NULL) printf("Member since %d/%d/%d\n", p->date->month, p->date->day, p->date->year);
		p = p->next;
	 
	}

	printf("\n## End of the List\n\n");
	
	return;
}
