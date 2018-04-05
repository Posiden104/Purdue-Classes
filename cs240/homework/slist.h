
struct SLEntry {
  char *name;
  char *value;
  struct SLEntry * next;
};

typedef struct SLEntry SLEntry;

struct SLList {
  SLEntry * head;
};

typedef struct SLList SLList;

SLList * sllist_create();
int sllist_insert( SLList *list, char *name, char *value );
int sllist_remove(SLList *list, char *name);
int sllist_last(SLList *list, char **pname, char **pvalue);
int sllist_contains(SLList *list, char *value, char *name);
void sllist_print(SLList *list);

