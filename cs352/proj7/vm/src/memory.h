#ifndef MEMORY_H
#define MEMORY_H

#include <stdlib.h>
#include "vmtypes.h"

#define GC_NOFREE       0 // the never-free garbage collector
#define GC_MARK_N_SWEEP 1 // the mark and sweep garbage collector
#define GC_COPYING      2 // the copying garbage collector

// the type of garbage collector to use
// TODO: change it to GC_MARK_N_SWEEP to test your code.
#define GC_VERSION GC_MARK_N_SWEEP

typedef enum {
  tag_String = 200,
  tag_RegisterFrame = 201,
  tag_Function = 202,
  tag_None = 255
} tag_t;

/* Returns a string identifying the memory system */
char* memory_get_identity(void);

/* Setup the memory allocator and garbage collector */
void memory_setup(size_t total_size);

/* Tear down the memory */
void memory_cleanup(void);

/* Get first memory address */
void* memory_get_start(void);

/* Get last memory address */
void* memory_get_end(void);

/* Set the heap start, following the code area */
void memory_set_heap_start(void* heap_start);

/* Allocate block, return physical pointer to the new block */
value_t* memory_allocate(tag_t tag, value_t size);

/* Unpack block size from a physical pointer */
value_t memory_get_block_size(value_t* block);

/* Unpack block tag from a physical pointer */
tag_t memory_get_block_tag(value_t* block);

#endif
