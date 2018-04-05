
//
// Implementation of a HashTable that stores void *
//
#include "HashTableVoid.h"

int HashTableVoid::hash(const char * key)
{
  int h = 0;
  const char * p = key;
  while (*p) {
    h += *p;
    p++;
  }
  return h % TableSize;
}

HashTableVoid::HashTableVoid()
{
  _buckets = (HashTableVoidEntry **) malloc(TableSize*sizeof(HashTableVoidEntry*));
  for ( int i = 0; i < TableSize; i++) {
    _buckets[i] = NULL;
  }
}

bool HashTableVoid::insertItem( const char * key, void * data)
{
  // Get hash bucket
  int h = hash(key);

  HashTableVoidEntry * e = _buckets[h];
  while (e!=NULL) {
    if (!strcmp(e->_key, key)) {
      // Entry found
      e->_data = data;
      return true;
    }
    e = e->_next;
  }

  // Entry not found. Add it.
  e = new HashTableVoidEntry;
  e->_key = strdup(key);
  e->_data = data;
  e->_next = _buckets[h];
  _buckets[h] = e;
  return false;
}

bool HashTableVoid::find( const char * key, void ** data)
{
  // Get hash bucket
  int h = hash(key);
  
  HashTableVoidEntry * e = _buckets[h];
  while (e!=NULL) {
    if (!strcmp(e->_key, key)) {
      // Entry found
      *data = e->_data;
      return true;
    }
    e = e->_next;
  }
  return false;
}

bool HashTableVoid::removeElement(const char * key)
{
  // Get hash bucket
  int h = hash(key);
  
  HashTableVoidEntry * e = _buckets[h];
  HashTableVoidEntry * prev = NULL;
  while (e!=NULL) {
    if (!strcmp(e->_key, key)) {
      // Entry found
      if (prev != NULL) {
	prev->_next = e->_next;
      }
      else {
	_buckets[h] = e->_next;
      }
      delete e;
      return true;
    }
    prev = e;
    e = e->_next;
  }
  return false;
}

// Visit all the entries in the hash table and call func(key,data,arg)
// in each entry.
void
HashTableVoid::visit( VisitFunc func, void * arg )
{
  for (int i = 0; i < TableSize; i++) {
    HashTableVoidEntry * entry = _buckets[i];
    while ( entry != NULL) {
      (*func)(entry->_key, entry->_data, arg);
      entry = entry->_next;
    }
  }
}
