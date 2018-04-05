#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef struct SLList {
	char *name;
	struct SLList *next;
} SLList;

struct SLList *head = NULL;

void addToList( char *name){
	if( head==NULL ){
		head = (SLList*)malloc(sizeof(SLList));
		head->next=NULL;
		head->name=name;
	} else {
		SLList *new = (SLList*)malloc(sizeof(SLList));
		new->name=name;
		new->next=head;
		head=new;
	}
}

void printList(){
	SLList *elem=head;
	while(elem!=NULL){
		printf("%s\n", elem->name);
		elem=elem->next;
	}
	printf("\n");
}

void addToEnd( char *name){
	SLList *test=head;
	SLList *last=NULL;
	while( test!=NULL ){
		last=test;
		test=test->next;
	}
	SLList *new=(SLList*)malloc(sizeof(SLList));
	new->name = name;
	new->next = NULL;
	if( last==NULL){
		head=new;
	} else {
		last->next=new;
	}
}

int removeElement( char*name ){
	SLList *elem=head;
	SLList *last=NULL;
	while( elem!=NULL ){
		if( 0==strcmp(name,elem->name) ){
			if(last==NULL){
				head=elem->next;
			} else {
				last->next=elem->next;
			}
			return 1;
		}
		last=elem;
		elem=elem->next;
	}
	return 0;
}

char *removeFirst(){
	if( head==NULL ){
		return NULL;
	}
	char*str=head->name;
	head=head->next;
	return str;
}

char *removeLast(){
	if( head==NULL ){
		return NULL;
	}
	SLList *elem=head;
	SLList *last=head;
	while( elem->next != NULL ){
		last=elem;
		elem=elem->next;
	}
	last->next=NULL; 
	return elem->name;
}



int main(){
	addToList("cde");
	addToList("ab");
	addToEnd("-b-a");
	addToEnd("qqq");
	printList();
	assert( removeElement("ab") );

	addToEnd("bqb");
	addToList("ied");
	printList();
	printf("%s\n",removeLast() );
	printf("%s\n\n",removeFirst() );

	printList();
}
