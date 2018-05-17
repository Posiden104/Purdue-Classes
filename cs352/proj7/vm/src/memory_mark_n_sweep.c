#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "memory.h"
#include "fail.h"
#include "engine.h"

#if GC_VERSION == GC_MARK_N_SWEEP

static void* memory_start = NULL;
static void* memory_end = NULL;

static uvalue_t* bitmap_start = NULL;

static value_t* heap_start = NULL;
static value_t* heap_end = NULL;
static value_t heap_start_v = 0;
static value_t heap_end_v = 0;
static value_t* heap_first_block = NULL;

#define FREE_LISTS_COUNT 32
static value_t* free_list_heads[FREE_LISTS_COUNT];

#define MIN_BLOCK_SIZE 1
#define HEADER_SIZE 1

// Header management

static value_t header_pack(tag_t tag, value_t size) {
  return (size << 8) | (value_t)tag;
}

static tag_t header_unpack_tag(value_t header) {
  return (tag_t)(header & 0xFF);
}

static value_t header_unpack_size(value_t header) {
  return header >> 8;
}

// Bitmap management

static int bitmap_is_bit_set(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  return (bitmap_start[word_index] & ((uvalue_t)1 << bit_index)) != 0;
}

static void bitmap_set_bit(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  bitmap_start[word_index] |= (uvalue_t)1 << bit_index;
}

static void bitmap_clear_bit(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  bitmap_start[word_index] &= ~((uvalue_t)1 << bit_index);
}

// Virtual <-> physical address translation

static void* addr_v_to_p(value_t v_addr) {
  return (char*)memory_start + v_addr;
}

static value_t addr_p_to_v(void* p_addr) {
  return (value_t)((char*)p_addr - (char*)memory_start);
}

// Free lists management

static value_t real_size(value_t size) {
  assert(0 <= size);
  return size < MIN_BLOCK_SIZE ? MIN_BLOCK_SIZE : size;
}

static unsigned int free_list_index(value_t size) {
  assert(0 <= size);
  return size >= FREE_LISTS_COUNT ? FREE_LISTS_COUNT - 1 : (unsigned int)size;
}

char* memory_get_identity() {
  return "mark & sweep garbage collector";
}

void memory_setup(size_t total_byte_size) {
  memory_start = malloc(total_byte_size);
  if (memory_start == NULL)
    fail("cannot allocate %zd bytes of memory", total_byte_size);
  memory_end = (char*)memory_start + total_byte_size;
}

void memory_cleanup() {
  assert(memory_start != NULL);
  free(memory_start);

  memory_start = memory_end = NULL;
  bitmap_start = NULL;
  heap_start = heap_end = NULL;
  heap_start_v = heap_end_v = 0;
  for (int l = 0; l < FREE_LISTS_COUNT; ++l)
    free_list_heads[l] = NULL;
}

void* memory_get_start() {
  return memory_start;
}

void* memory_get_end() {
  return memory_end;
}

void memory_set_heap_start(void* ptr) {
  assert(memory_start <= ptr && ptr < memory_end);

  const size_t bh_size =
    (size_t)((char*)memory_end - (char*)ptr) / sizeof(value_t);

  const size_t bitmap_size = (bh_size - 1) / (VALUE_BITS + 1) + 1;
  const size_t heap_size = bh_size - bitmap_size;

  bitmap_start = ptr;
  memset(bitmap_start, 0, bitmap_size * sizeof(value_t));

  heap_start = (value_t*)bitmap_start + bitmap_size;
  heap_end = heap_start + heap_size;
  assert(heap_end == memory_end);

  heap_start_v = addr_p_to_v(heap_start);
  heap_end_v = addr_p_to_v(heap_end);

  heap_first_block = heap_start + HEADER_SIZE;
  const value_t initial_block_size = (value_t)(heap_end - heap_first_block);
  heap_first_block[-1] = header_pack(tag_None, initial_block_size);
  heap_first_block[0] = 0;

  for (int l = 0; l < FREE_LISTS_COUNT - 1; ++l)
    free_list_heads[l] = memory_start;
  free_list_heads[FREE_LISTS_COUNT - 1] = heap_first_block;
}


/*
    ██   ██ ███████ ██      ██████  ███████ ██████  ███████
    ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
    ███████ █████   ██      ██████  █████   ██████  ███████
    ██   ██ ██      ██      ██      ██      ██   ██      ██
    ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
*/

value_t addToFreeLists(value_t* ptr, value_t size){
  assert(heap_start <= ptr && ptr < heap_end);
  value_t index = free_list_index(size);
  value_t* prev_head = free_list_heads[index];
  ptr[0] = addr_p_to_v(prev_head);
  free_list_heads[index] = ptr;
  return index;
}

int isInHeap(value_t* ptr){
  if(heap_start <= ptr && ptr < heap_end){
    return 1;
  }
  return 0;
}

/*
     █████  ██      ██       ██████   ██████  █████  ████████ ███████
    ██   ██ ██      ██      ██    ██ ██      ██   ██    ██    ██
    ███████ ██      ██      ██    ██ ██      ███████    ██    █████
    ██   ██ ██      ██      ██    ██ ██      ██   ██    ██    ██
    ██   ██ ███████ ███████  ██████   ██████ ██   ██    ██    ███████
*/
value_t* allocate_split_block(value_t realSize, value_t index, int offset, int addToFreeList){
  // printf("Split block\n");
  value_t* reservedBlock = free_list_heads[index];
  int isHead = 1;
  if(index == FREE_LISTS_COUNT - 1 && offset > 0){
    // offset for last list
    value_t* prev = reservedBlock;
    isHead = 0;
    for(int i = 0; i <= offset; i++){
      prev = reservedBlock;
      reservedBlock = addr_v_to_p(reservedBlock[0]);
    }
    // extract reservedBlock from free list
    prev[0] = reservedBlock[0];
  }

  value_t remaining_space = header_unpack_size(reservedBlock[-1]) - realSize - HEADER_SIZE;
  // printf("remaining space: %i\n", remaining_space);
  assert(remaining_space >= MIN_BLOCK_SIZE);
  value_t* leftoverBlock = reservedBlock + realSize + HEADER_SIZE;

  // new pointers
  reservedBlock[-1] = header_pack(tag_None, realSize);
  leftoverBlock[-1] = header_pack(tag_None, remaining_space);

  if(isHead == 1){
    free_list_heads[index] = addr_v_to_p(reservedBlock[0]);
  }

  if(addToFreeList == 1) addToFreeLists(reservedBlock, realSize);
  addToFreeLists(leftoverBlock, remaining_space);
  return reservedBlock;
}

int allocate_find_offset(value_t realSize){
  // printf("find offset\n");
  value_t index = FREE_LISTS_COUNT - 1;
  value_t* tmp = free_list_heads[index];
  if(tmp == memory_start) return -1;
  value_t blockSize = header_unpack_size(tmp[-1]);
  int ret = 0;

  // find block that is big enough
  while(blockSize < realSize && tmp != memory_start){
    tmp = addr_v_to_p(tmp[0]);
    if(isInHeap(tmp) != 1) break;
    blockSize = header_unpack_size(tmp[-1]);
    ret++;
  }
  if(tmp == memory_start){
    return -1;
  }
  return ret;
}

// exact size only
value_t* allocate_with_exact(tag_t tag, value_t realSize){
  // printf("allocate exact\n");
  if(free_list_heads[realSize] != memory_start){
    value_t* ret = free_list_heads[realSize];
    free_list_heads[realSize] = addr_v_to_p(ret[0]);
    ret[-1] = header_pack(tag, realSize);

    assert(heap_start <= ret && ret < heap_end);
    bitmap_set_bit(ret);
    return ret;
  }
  return NULL;
}

value_t* allocate_with_last(tag_t tag, value_t realSize){
  // printf("allocate last\n");
  int offset = allocate_find_offset(realSize);
  if(offset == -1) return NULL;
  value_t* ret = allocate_split_block(realSize, FREE_LISTS_COUNT - 1, offset, 0);
  ret[-1] = header_pack(tag, realSize);

  assert(heap_start <= ret && ret < heap_end);
  bitmap_set_bit(ret);
  return ret;
}

value_t allocate_find_free_list(value_t realSize){
  // printf("find free list\n");
  value_t index = free_list_index(realSize);
  if(free_list_heads[index] != memory_start){
    return index;
  }
  index = realSize + MIN_BLOCK_SIZE + HEADER_SIZE;
  for(; index < FREE_LISTS_COUNT; index += MIN_BLOCK_SIZE){
    if(free_list_heads[index] != memory_start){
      return index;
    }
  }
  return -1;
}

/*
     █████  ██      ██       ██████   ██████  █████  ████████ ███████     ██████   █████  ███████ ███████
    ██   ██ ██      ██      ██    ██ ██      ██   ██    ██    ██          ██   ██ ██   ██ ██      ██
    ███████ ██      ██      ██    ██ ██      ███████    ██    █████       ██████  ███████ ███████ █████
    ██   ██ ██      ██      ██    ██ ██      ██   ██    ██    ██          ██   ██ ██   ██      ██ ██
    ██   ██ ███████ ███████  ██████   ██████ ██   ██    ██    ███████     ██████  ██   ██ ███████ ███████
*/

int highest = -1;
int lowest = 9999999;

static value_t* allocate(tag_t tag, value_t size) {
  // TODO
  value_t* ret;

  if(size > highest) {
    highest = size;
    printf("highest: %i\n", highest);
  }

  if(size < lowest) {
    lowest = size;
    printf("lowest: %i\n", lowest);
  }


  assert(0 <= size);
  value_t realSize = real_size(size);
  assert(MIN_BLOCK_SIZE <= realSize);

  if(realSize >= FREE_LISTS_COUNT - 1){
    ret = allocate_with_last(tag, realSize);
  } else {
    value_t index = allocate_find_free_list(realSize);
    if(index == -1) return NULL;

    if(index < FREE_LISTS_COUNT - 1){
      // split block
      if(index > realSize) allocate_split_block(realSize, index, 0, 1);
      ret = allocate_with_exact(tag, realSize);
    } else {
      int offset = allocate_find_offset(realSize);
      if(offset == -1) return NULL;
      allocate_split_block(realSize, index, offset, 1);
      ret = allocate_with_exact(tag, realSize);
    }
  }

  if(ret != NULL)
    assert(memory_get_block_size(ret) >= MIN_BLOCK_SIZE);

  if(size == 0)
    ret[-1] = header_pack(tag, size);

  return ret;
}

/*
    ███    ███  █████  ██████  ██   ██
    ████  ████ ██   ██ ██   ██ ██  ██
    ██ ████ ██ ███████ ██████  █████
    ██  ██  ██ ██   ██ ██   ██ ██  ██
    ██      ██ ██   ██ ██   ██ ██   ██
*/

static void mark(value_t* block) {
  // TODO
  if(isInHeap(block) != 1) return;
  bitmap_clear_bit(block);
  value_t size = header_unpack_size(block[-1]);

  // look at all elements of this block
  for(int i = 0; i < size; i++){
    value_t el = block[i];
    if((el & 0x03) == 0){
      // value is a virtual address
      value_t* addr = addr_v_to_p(el);
      if(isInHeap(addr) == 1 && bitmap_is_bit_set(addr)){
        mark(addr);
      }
    }
  }
}

/*
    ███████ ██     ██ ███████ ███████ ██████
    ██      ██     ██ ██      ██      ██   ██
    ███████ ██  █  ██ █████   █████   ██████
         ██ ██ ███ ██ ██      ██      ██
    ███████  ███ ███  ███████ ███████ ██
*/

static void sweep() {

  printf("%s\n", "sweeping");

  // TODO
  value_t* ptr = heap_start;
  value_t* prevFree = memory_start;
  value_t prevIndex = 0;
  int justFreed = 0;

  for (int l = 0; l < FREE_LISTS_COUNT - 1; ++l){
    // mark all freelists for freeing
    value_t* tmp = free_list_heads[l];
    while(tmp != memory_start){
      assert(heap_start <= tmp && tmp < heap_end);
      bitmap_set_bit(tmp);
      tmp = addr_v_to_p(tmp[0]);
    }
    // empty the free lists
    free_list_heads[l] = memory_start;
  }

  // look at every block
  while(ptr != heap_end){
    if(isInHeap(ptr) == 0){
      ptr = memory_end;
    } else {
      value_t size = real_size(header_unpack_size(ptr[-1]));
      assert(isInHeap(ptr) == 1 || ptr == memory_end);
      value_t* nextAddr = ptr + size + HEADER_SIZE;
      if(!bitmap_is_bit_set(ptr)){
        justFreed = 0;
        assert(heap_start <= ptr && ptr < heap_end);
        bitmap_set_bit(ptr);
      } else {
        // reset the bit
        bitmap_clear_bit(ptr);

        // coalescing
        if(justFreed == 1){
          // remove previous from its free list
          free_list_heads[prevIndex] = addr_v_to_p(prevFree[0]);
          value_t prevSize = header_unpack_size(prevFree[-1]);

          // add block to the previous
          size = prevSize + size + HEADER_SIZE;
          prevFree[-1] = header_pack(tag_None, size);
          ptr = prevFree;
        }
        // add to free lists
        prevIndex = addToFreeLists(ptr, size);
        prevFree = ptr;
        justFreed = 1;
      }
      ptr = nextAddr;
    }
  }
}

value_t* memory_allocate(tag_t tag, value_t size) {
  value_t* first_try = allocate(tag, size);
  if (first_try != NULL)
    return first_try;

  printf("Marking\n");
  value_t* lb = engine_get_Lb();
  if (lb != memory_start) mark(lb);
  value_t* ib = engine_get_Ib();
  if (ib != memory_start) mark(ib);
  value_t* ob = engine_get_Ob();
  if (ob != memory_start) mark(ob);

  sweep();

  value_t* second_try = allocate(tag, size);
  if (second_try != NULL)
    return second_try;

  fail("\ncannot allocate %d words of memory, even after GC\n", size);
}

value_t memory_get_block_size(value_t* block) {
  return header_unpack_size(block[-1]);
}

tag_t memory_get_block_tag(value_t* block) {
  return header_unpack_tag(block[-1]);
}

#endif
