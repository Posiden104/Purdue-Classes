
#include <stdio.h>
#include <stdlib.h>

#include "slist.h"

void
sllist_create(SLList * list)
{
	list->head = NULL;
}

// Add int value to the end of list. The values may be repeated.
int
sllist_insert( SLList *list,char *name, char *value )  {
	if(*name == '\0') return 0;
	SLEntry *s = (SLEntry*) malloc(sizeof(SLEntry));
	SLEntry *p = list->head;
	s->value = value;
	s->name = name;
	s->next = NULL;
	if(p == NULL) {
		list->head = s;
		return;
	}
	SLEntry *prev = list->head;
	while(p->next != NULL) {
		if(!strcmp(name, p->name)) {
			p->value = value;
			free(s);
			return 1;
		}
		prev = p;
		p = p->next;
	}
	p->next = s;

	return 1;
}


// Remove first occurrence of value in the list. It returns 1 if value is found or 0 otherwise.
int sllist_remove(SLList *list, char* name) {
	SLEntry *s = list->head;
	SLEntry *n = list->head;
	if(!strcmp(list->head->name, name)) {
		n = list->head;
		list->head = list->head->next;
		free(n);
		return 1;
	}
	while(s->next != NULL) {
		if(!strcmp(s->next->name, name)) {
			n = s->next;
			s->next = s->next->next;
			free(n);
			return 1;
		}
		s = s->next;
	}
	return 0;
}

int sllist_last(SLList, char **name, char**value){
	if(list->head == NULL) return 0;

	return 1;
}

int sllist_contains(SLList *list, char * value, char *name) {
	SLEntry *p = list->head;
	if(!strcmp(list->head->value, value)||!strcmp(p->name, name)) return 1;
	while(p != NULL) {
		if(!strcmp(p->value, value)||!strcmp(p->name, name)) return 1;
		p = p->next;
	}
	return 0;
}

void
sllist_print(SLList *list)
{
	// Move to the end
	SLEntry * e = list->head;

	printf("--- List ---\n");
	while (e!= NULL) {
		printf("val=%d\n", e->value);
		e = e->next;
	}
}
