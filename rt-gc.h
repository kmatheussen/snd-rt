
#include <rt-various.h>

// stalin uses atomic memory for void* pointers.
#define GC_malloc_atomic(size) tar_alloc(heap,size)

#define GC_malloc(size) tar_alloc(heap,size)

// This one too.
//#define GC_malloc_atomic_uncollectable(size) tar_malloc_atomic_uncollectable(size)
#define GC_malloc_atomic_uncollectable(size) tar_alloc(heap,size)

#define GC_malloc_uncollectable(size) tar_alloc_uncollectable(size)

// Hmm, only works if its atomic uncollectable mem. Not good.
//#define GC_free(mem) tar_free_atomic_uncollectable(mem)
// Same reason. Just make it a dummy:
#define GC_free(mem) /* */
