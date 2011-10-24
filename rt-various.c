
#include <mus-config.h>

#ifdef WITH_RT

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <errno.h>
#include <sched.h>
#include <pthread.h>

#include "rt-various.h"

#include "_sndlib.h"
#include "clm.h"


/* Stalin stuff. */

int fix_stalin_c_source(char* infile, char *outfile){
  char temp[10000];
  FILE *file=fopen(infile,"r");
  FILE *out=fopen(outfile,"w");
  while(fgets(temp,9990,file)){
    if(0
       || temp[0]==' '
       || temp[0]=='#'
       || temp[0]=='{'
       || (!isalpha(temp[0]))
       )
      if(!strcmp("#include <gc.h>\n",temp))
        fputs("#include <rt-gc.h>\n",out);
      else
        fputs(temp,out);
    else{
      int len=strlen(temp);
      if(0
         || (temp[len-3]!='}' && temp[len-2]==';')
         || (temp[len-3]==';' && temp[len-2]=='}')
         || (temp[len-4]=='}' && temp[len-3]=='}' && temp[len-2]==';')
         || temp[len-2]=='/'
         || temp[len-2]==')'
         ){
        fprintf(out,"static ");
        if(!strcmp("int main(void)\n",temp))
          fputs("int schememain(void)\n",out);
        else
          fputs(temp,out);
      }else
        fputs(temp,out);
    }
  }
  fclose(out);
  fclose(file);
  return 0;
}


/* Rollendurchmesserzeitsammler stuff. */

static bool rollendurch_inited=false;
static pthread_t main_thread;

void init_rollendurchmesserzeitsammler(int a,int b,int c,int d,float e){
  main_thread = pthread_self();
  tar_init(a,
	   b,
	   c,
           d,
           e);
  rollendurch_inited = true;
}


// Force linking of various functions in rollendurchmesserzeitsammler.
void rt_various_dummy(void){
  fprintf(stderr,"It is an error to call this function.\n");

  tar_get_dynamic_roots_for(NULL,NULL,NULL);
  tar_init_block(1.0f);
  tar_before_using_heap(NULL);
  tar_after_using_heap(NULL);
  tar_add_root(NULL,NULL,NULL);
  tar_add_root_concurrently(NULL,NULL,NULL);
  tar_start_gc(NULL);
  tar_create_heap();
  tar_delete_heap(NULL,false);
  tar_alloc(NULL,0);
  tar_alloc_atomic(NULL,0);
  tar_get_used_mem(NULL);
  tar_get_used_atomic_mem(NULL);

  //tar_malloc_atomic_uncollectable(0);
  //tar_calloc_atomic_uncollectable(0,0);
  //tar_free_atomic_uncollectable(NULL);
  //  tar_printstats();
  //  tar_resetstats();
}



/* clm stuff */

static tar_heap_t *clm_tar_heap=NULL;
static error_func_t clm_error_func=NULL;

static int is_nonrealtime_thread(void){
  // printf("%p, %d, %d\n",clm_tar_heap, rollendurch_inited, pthread_equal(pthread_self(),main_thread));
  return (clm_tar_heap==NULL || rollendurch_inited==false || pthread_equal(pthread_self(),main_thread)!=0);
}

tar_heap_t *clm_set_tar_heap(tar_heap_t *new_heap){
    tar_heap_t *ret=clm_tar_heap;
    clm_tar_heap=new_heap;
    return ret;
}

error_func_t clm_set_error_func(error_func_t new_error){
  error_func_t ret=clm_error_func;
  clm_error_func=new_error;
  return ret;
}


int rt_mus_error(int type,const char* fmt,...){
  int size;
  va_list argp;
  static char string[1024]; //Should be static, stack is limited in realtime.

  va_start(argp,fmt);
  vsprintf(string,fmt,argp);
  va_end(argp);

  if(clm_error_func==NULL)
    return mus_error(type,string);
  else
    clm_error_func(string);
  return -1;
}


void* clm_calloc_atomic(int num,size_t size,const char* what){
  if(num*size==0)
    return NULL;
  if(is_nonrealtime_thread()){
    return calloc(num,size);
  }else{
    void *ret=tar_alloc_atomic(clm_tar_heap,num*size);
    //memset(ret,0,num*size);
    //printf("heap: %p. Size: %d, ret: %p\n",clm_tar_heap, num*size,ret);
    if(ret==NULL && num>0 && size>0){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    return ret;
  }
}


void* clm_calloc(int num,size_t size,const char* what){
  if(is_nonrealtime_thread()){
    return calloc(num,size);
  }else{
    void *ret=tar_alloc(clm_tar_heap,num*size);
    if(ret==NULL && num>0 && size>0){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    return ret;
  }
}

void* clm_malloc_atomic(size_t size,const char* what){
  if(is_nonrealtime_thread()){
    return malloc(size);
  }else{
    void *ret=tar_alloc_atomic(clm_tar_heap,size);
    if(ret==NULL && size>0){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    return ret;
  }
}

void* clm_malloc(size_t size,const char* what){
  if(is_nonrealtime_thread()){
    return malloc(size);
  }else{
    void *ret=tar_alloc(clm_tar_heap,size);
    if(ret==NULL && size>0){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    return ret;
  }
}

void* clm_realloc(void* old,size_t newsize){
  if(is_nonrealtime_thread()){
    return realloc(old,newsize);
  }else{
    return memcpy(clm_malloc(newsize,"realloc"),old,newsize);
  }
}


void clm_free(void* p){
  if(is_nonrealtime_thread()){
    free(p);
  }
}

#endif // WITH_RT
