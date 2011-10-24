
# include <rollendurchmesserzeitsammler.h>

#ifdef __cplusplus
#  define LANGSPEC "C"
#else
#  define LANGSPEC
#endif


extern LANGSPEC int fix_stalin_c_source(char* infile, char *outfile);

extern LANGSPEC void init_rollendurchmesserzeitsammler(int a,int b,int c,int d,float e);

/* hbgc->rollendurchmesserzeitsammler */

// disabled for now since the returned mem can be used to store void* pointers.
//#define GC_malloc_atomic(size) tar_alloc_atomic(heap,size)

#if 0
#endif

// In clm.c returns the old heap.
//tar_heap_t *clm_set_tar_heap(tar_heap_t *new_heap);
extern LANGSPEC tar_heap_t *clm_set_tar_heap(tar_heap_t *new_heap);

typedef void (*error_func_t)(char*);

extern LANGSPEC error_func_t clm_set_error_func(error_func_t new_error);

extern LANGSPEC int rt_mus_error(int type,const char* fmt,...);

extern LANGSPEC void* clm_calloc_atomic(int num,size_t size,const char* what);
extern LANGSPEC void* clm_calloc(int num,size_t size,const char* what);
extern LANGSPEC void* clm_malloc_atomic(size_t size,const char* what);
extern LANGSPEC void* clm_malloc(size_t size,const char* what);

extern LANGSPEC void* clm_realloc(void* old,size_t newsize);
extern LANGSPEC void clm_free(void* p);


