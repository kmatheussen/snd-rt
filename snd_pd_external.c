/* --------------------------- snd_pd_external  ------------------------------- */
/* Made by Kjetil S. Matheussen, 2005.                                          */
/*                                                                              */
/* Based on:                                                                    */
/*                                                                              */
/*     --------------------------- k_guile  ----------------------------------- */
/*   ;; Kjetil S. Matheussen, 2004.                                             */
/*                                                                              */
/* This program is free software; you can redistribute it and/or                */
/* modify it under the terms of the GNU General Public License                  */
/* as published by the Free Software Foundation; either version 2               */
/* of the License, or (at your option) any later version.                       */
/*                                                                              */
/* This program is distributed in the hope that it will be useful,              */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of               */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                */
/* GNU General Public License for more details.                                 */
/*                                                                              */
/* You should have received a copy of the GNU General Public License            */
/* along with this program; if not, write to the Free Software                  */
/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  */
/*                                                                              */
/* ---------------------------------------------------------------------------- */


/* INFO: SND runs in its own thread. Communication between PD and SND happens
   by sending messages back and forth on two ringbuffer. All functions running
   in the PD thread (ie. main program) have the prefix "pd0_", and all functions
   running in the SND thread have the prefix "snd0_".

   TOPIC: Is gensym() (in m_class.c) thread-safe? It looks so, but I'm not sure.
    (no it's not)

   TOPIC: scm_gc is called here and there, and also scm_cons. Shouldn't scm_remember_up_to_here be called?
   TOPIC: Perhaps 1.0 is too small in pd0_das_dispatcher
*/


#include "snd.h"


/* Need some more macros. */

#define POINTER_P(x) (((int) (x) & 3) == 0)
#define INTEGER_P(x) (! POINTER_P (x))

#define GET_INTEGER SCM_INUM 
#define MAKE_INTEGER  SCM_MAKINUM

#define MAKE_STRING(a) scm_mem2string(a,strlen(a))
#define EVAL(a) scm_eval_string(MAKE_STRING(a))
#define CATCH_EVAL(a,error) snd_catch_any(eval_str_wrapper,a,error)

#define MAKE_SYM(a) snd0_gensym(SCM_SYMBOL_CHARS(a))

#if 0
#  define MAKE_STRING_rt(a) scm_protect_object(scm_mem2string(a,strlen(a)))
#  define GET_POINTER3(a) (scm_is_false(a)?NULL:(void *)scm_num2ulong(a,0,"GET_POINTER3()"))
#  define GET_POINTER(a) (scm_is_false(a)?NULL:GET_POINTER3(SCM_CAR(SCM_CDR(a))))
#  define GET_POINTER2(a) GET_POINTER(a)
#  define MAKE_POINTER(a) scm_cons(MAKE_STRING_rt("A_POINTER"),scm_cons(scm_ulong2num((unsigned long)a),SCM_EOL))

#  define GET_POINTER_rt(a) GET_POINTER(a)

#else

#  define MAKE_POINTER(a) scm_ulong2num((unsigned long)a)
#  define GET_POINTER(a) (void*)scm_num2ulong(a,0,"GET_POINTER()")
#  define GET_POINTER_rt(a) (void *)scm_num2ulong(SCM_CAR(SCM_CDR(a)),0,"GET_POINTER()")

#endif

#define GET_X(a) ((t_snd_pd *)GET_POINTER(a))

#define RU_ return SCM_UNSPECIFIED


#include <m_pd.h>
//#include <s_stuff.h>
#include <pthread.h>
#include <jack/ringbuffer.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <stdarg.h>

#include "snd_pd_external.h"

static char *version = "Snd " SND_VERSION " made by Bill Schottstaedt, bil@ccrma.stanford.edu.\nSnd as a PD external made by Kjetil S. Matheussen, kjetil@ccrma.stanford.edu.";

static t_class *snd_pd_class, *snd_pd_workaroundclass;

static SCM eval_string_func;

static t_clock *snd_pd_clock;

static pthread_t pthread={0};
static pthread_t pthread_repl={0};
static jack_ringbuffer_t *rb_snd_to_pd;
static jack_ringbuffer_t *rb_pd_to_snd;
static pthread_cond_t thread_cond=PTHREAD_COND_INITIALIZER;
static pthread_mutex_t thread_mutex=PTHREAD_MUTEX_INITIALIZER;

struct dispatch{
  void (*func)(struct dispatch *d);
  t_snd_pd *x;
  union{
    char *filename;
    int inlet_num;
    int outlet_num;
    t_class **class;
    t_snd_pd_workaround *x2;
    SCM func;
  }data;
  union{
    t_float val;
    int list_length;
    t_symbol *symbol;
    SCM func;
  }data2;
};



t_symbol *snd0_gensym(char *symbol){
  t_symbol *ret;
    struct sched_param par;

  {
    par.sched_priority = sched_get_priority_max(SCHED_FIFO);
    if(sched_setscheduler(0,SCHED_FIFO,&par)==-1){
      post("snd0_gensym: Unable to aquire SCHED_FIFO priority");
    }
  }{
    sys_lock();{

      ret=gensym(symbol);

    }sys_unlock();
  }sched_setscheduler(0,SCHED_OTHER,&par);

  return ret;
}



/*****************************************************************************************************
 *****************************************************************************************************
 *    Functions to evaluate large amount of scheme code from C.
 *****************************************************************************************************
 *****************************************************************************************************/

static char *evalstring=NULL;
static void snd0_eval2(char *string){
  char *new;
  if(evalstring==NULL){
    new=malloc(strlen(string)+1);
    sprintf(new,"%s",string);
  }else{
    new=malloc(strlen(evalstring)+strlen(string)+1);
    sprintf(new,"%s%s",evalstring,string);
    free(evalstring);
  }
  evalstring=new;
}
static void snd0_eval_file(char *filename){
  FILE *file=fopen(filename,"r");
  char line[50000];
  if(file==NULL){
    fprintf(stderr,"Error! snd_pd_external.c/snd0_eval_file: Could not open file \"%s\".\n",filename);
    return;
  }
  for(;;){
    int c=fgetc(file);
    if(c==EOF) break;
    ungetc(c,file);
    fgets(line,49999,file);
    snd0_eval2(line);
  }
  fclose(file);
}
static SCM snd0_eval_do(void){
  //post(evalstring);
  SCM ret=EVAL(evalstring);
  free(evalstring);
  evalstring=NULL;
  return ret;
}




/*****************************************************************************************************
 *****************************************************************************************************
 *    Pd dispatcher. pd0_das_dispatcher is called pretty often from pd (~1000 times per second) 
 *    and checks the snd->pd ringbuffer.
 *****************************************************************************************************
 *****************************************************************************************************/

static void pd0_das_dispatcher(void *something){
  jack_ringbuffer_t *rb=rb_snd_to_pd;
  struct dispatch d;
  int num_nondispatched=128; // Make sure too much work is not done at once in the realtime thread.

  while(jack_ringbuffer_read_space(rb) >= sizeof(struct dispatch)){
    //printf("pd0: I got something to dispatch!\n");
    jack_ringbuffer_read (rb, (char *)&d, sizeof(struct dispatch));
    d.func(&d);
    num_nondispatched--;
    if(num_nondispatched==0)
      break;
  }

  clock_delay(snd_pd_clock, 1.0);
}


static void snd0_send_message(struct dispatch *d){
  jack_ringbuffer_t *rb=rb_snd_to_pd;
  size_t bytes;
  int retries=0;

 tryagain:
  //  if(jack_ringbuffer_write_space(rb)<sizeof(struct dispatch)){
    //scm_gc();
    while(jack_ringbuffer_write_space(rb)<sizeof(struct dispatch)){
      post("Warning. Ringbuffer full. Could not send message to PD. Trying again in one second.");
      sleep(1);
    }
    //  }
  if((bytes=jack_ringbuffer_write (rb, (char*)d, sizeof(struct dispatch))) < sizeof(struct dispatch)){
    if(bytes>0)
      post("Catastrophe snd0_send_message!!! (please report this to k.s.matheussen@notam02.no");
    goto tryagain;
  }
}



/*****************************************************************************************************
 *****************************************************************************************************
 *    Snd dispatcher
 *****************************************************************************************************
 *****************************************************************************************************/

static jack_ringbuffer_t *rb_repl;

static void snd0_das_dispatcher(void){
  jack_ringbuffer_t *rb=rb_pd_to_snd;
  char repl_some[500];
  int bytes;
  struct dispatch d;
  while(1){
    //fprintf(stderr,"snd0: waiting for something to dispatch\n");
    while(jack_ringbuffer_read_space(rb) >= sizeof(struct dispatch)){
      //fprintf(stderr,"snd0: I got something to dispatch\n");
      jack_ringbuffer_read (rb, (char *)&d, sizeof(struct dispatch));
      d.func(&d);
    }
    while((bytes=jack_ringbuffer_read(rb_repl,repl_some,498))>0){
      repl_some[bytes]=0;
      snd_eval_stdin_str(repl_some);
    }
    pthread_cond_wait (&thread_cond, &thread_mutex);
  }
}


static void *read_eval_print_loop(void *arg){
  char repl_some[5000];
  int len;
  while(1){
    fgets(repl_some,4950,stdin);
    len=strlen(repl_some);
    if(len>4950)
      fprintf(stderr,"WARNING, repl buffer probably too small (%d>4950). Please report to k.s.matheussen@notam02.no\n",len);
    while(jack_ringbuffer_write_space(rb_repl)<len){
      usleep(50000);
    }
    jack_ringbuffer_write(rb_repl,repl_some,len);
    pthread_cond_broadcast(&thread_cond);
  }
  return NULL;
}



static void pd0_send_message(struct dispatch *d){
  jack_ringbuffer_t *rb=rb_pd_to_snd;
  size_t bytes;

  if(jack_ringbuffer_write_space(rb)<sizeof(struct dispatch)){
    post("Error. Ringbuffer full. Could not send message to SND.");
    post("This behaviour could be changed. Please send me a note if giving up is the wrong behaviour for you. k.s.matheussen@notam02.no");
    goto end;
  }

  if( (bytes=jack_ringbuffer_write (rb, (char*)d, sizeof(struct dispatch))) < sizeof(struct dispatch)){
    if(bytes>0)
      post("Catastrophe pd0_send_message!!! (please report this to k.s.matheussen@notam02.no");
  }

 end:
  pthread_cond_broadcast(&thread_cond);

}




/*****************************************************************************************************
 *****************************************************************************************************
 *    Got data from PD, on its way to Guile. Called either via bind or an inlet.
 *****************************************************************************************************
 *****************************************************************************************************/

static jack_ringbuffer_t *rb_anything;

static void snd0_anything_do(struct dispatch *d){
  int lokke;
  t_int argc;
  SCM applyarg=SCM_EOL;
  t_symbol *s;
  int index=d->data.inlet_num;


  jack_ringbuffer_read(rb_anything,(char *)&s,sizeof(t_symbol*));
  jack_ringbuffer_read(rb_anything,(char *)&argc,sizeof(t_int));

  for(lokke=argc-1;lokke>=0;lokke--){
    SCM to=SCM_BOOL_F;
    t_atom argv;
    jack_ringbuffer_read(rb_anything,(char *)&argv,sizeof(t_atom));
    switch(argv.a_type){
    case A_NULL:
      to=SCM_EOL;
      break;
    case A_FLOAT:
      to=scm_make_real(atom_getfloat(&argv));
      break;
    case A_SYMBOL:
      to=scm_string_to_symbol(MAKE_STRING(atom_getsymbol(&argv)->s_name));
      break;
    default:
      post("Strange");
      break;
    }
    applyarg=scm_cons(to,applyarg);
  }

  if(index>=0){
    // Inlet
    //scm_call_3(d->x->inlet_func,MAKE_INTEGER(index),scm_string_to_symbol(MAKE_STRING(s->s_name)),applyarg);
    g_call3(d->x->inlet_func,MAKE_INTEGER(index),scm_string_to_symbol(MAKE_STRING(s->s_name)),applyarg,"Inlet\n");
  }else{
    // Binding
    if(s!=&s_float && s!=&s_list && s!=&s_symbol){
      applyarg=scm_cons(scm_string_to_symbol(MAKE_STRING(s->s_name)),applyarg);
    }
    if(s!=&s_list && GET_INTEGER(scm_length(applyarg))==1)
      applyarg=SCM_CAR(applyarg);
    //scm_call_1(d->data2.func,applyarg);
    g_call1(d->data2.func,applyarg,"Binding or inlet.\n");
  }
}

static void pd0_anything_do(t_snd_pd *x,int index,SCM func,t_symbol *s, t_int argc, t_atom* argv){
  int lokke;

  if(jack_ringbuffer_write_space(rb_anything) < sizeof(t_symbol*)+sizeof(t_int)+(sizeof(t_atom)*argc)){
    post("Error in function pd0_anything_do. Ringbuffer full. Can't send message to SND.");
    return;
  }

  // Yes, it can be done in many steps. (as long as pd is singlethreaded at least, but I think it is....)
  jack_ringbuffer_write(rb_anything,(char *)&s,sizeof(t_symbol*));
  jack_ringbuffer_write(rb_anything,(char *)&argc,sizeof(t_int));

  for(lokke=argc-1;lokke>=0;lokke--){
    jack_ringbuffer_write(rb_anything,(char *)&argv[lokke],sizeof(t_atom));
  }

  {
    struct dispatch d;
    d.func=snd0_anything_do;
    d.x=x;
    d.data.inlet_num=index;
    d.data2.func=func;
    pd0_send_message(&d);
  }

}

// Handles inlet>0 and bindings
static void pd0_anything(t_snd_pd_workaround *x2,t_symbol *s, t_int argc, t_atom* argv){
  if(x2->x!=NULL && x2->x->isworking==false){
    post("Object not functional. No scheme file was loaded.");
    return;
  }

  if(x2->index>=0){
    // Inlet
    pd0_anything_do(x2->x,x2->index,0,s,argc,argv);
  }else{
    // Binding
    pd0_anything_do(NULL,x2->index,x2->func,s,argc,argv);
  }
}

// Handles first inlet
static void pd0_anything_first(t_snd_pd *x,t_symbol *s, t_int argc, t_atom* argv){
  if(x->isworking==false){
    post("Object not functional. No scheme file was loaded.");
    return;
  }

  pd0_anything_do(x,0,0,s,argc,argv);
}






/*****************************************************************************************************
 *****************************************************************************************************
 *    Binding and unbinding. Called from the guile side. 
 *****************************************************************************************************
 *****************************************************************************************************/

static volatile int snd_pd_is_binding=0;
static volatile int snd_pd_is_unbinding=0;
static t_snd_pd_workaround *snd_pd_bind_x2;

static void pd0_bind(struct dispatch *d){
  t_snd_pd_workaround *x2;
  x2=(t_snd_pd_workaround*)pd_new(snd_pd_workaroundclass);
  x2->x=NULL;
  x2->index=-1;
  x2->func=d->data.func;
  pd_bind((t_pd*)x2, d->data2.symbol);

  //post("pd0_bind \"%s\"",d->data2.symbol->s_name);
  snd_pd_bind_x2=x2;
  snd_pd_is_binding=0;
}

static SCM snd0_bind(SCM symname,SCM func){
  struct dispatch d;

  //post("Trying to bind \"%s\"",SCM_SYMBOL_CHARS(symname));
  snd_pd_is_binding=1;

  scm_protect_object(func);

  d.func=pd0_bind;
  d.data.func=func;
  d.data2.symbol=MAKE_SYM(symname);
  snd0_send_message(&d);

  if(snd_pd_is_binding!=0){
    int num_retries=0;
    scm_gc();
    while(snd_pd_is_binding!=0){
      num_retries++;
      if(num_retries>(1000000/50000)*5){
	post("snd0_bind: Waited 5 seconds for pd. Something is probably wrong. Could not bind(?).");
	RU_;
      }
      usleep(50000);
    }
  }

  return MAKE_POINTER(snd_pd_bind_x2);
}

static void pd0_unbind(struct dispatch *d){
  pd_unbind((t_pd *)d->data.x2,d->data2.symbol);
  pd_free((t_pd*)d->data.x2);
  snd_pd_is_unbinding=0;
}

static SCM snd0_unbind(SCM scm_x2,SCM symname){
  struct dispatch d;
  SCM func;

  snd_pd_is_unbinding=1;

  d.func=pd0_unbind;
  d.data.x2=GET_POINTER(scm_x2);
  d.data2.symbol=MAKE_SYM(symname);
  func=d.data.x2->func;
  snd0_send_message(&d);

  if(snd_pd_is_unbinding!=0){
    int num_retries=0;
    scm_gc();
    while(snd_pd_is_unbinding!=0){
      num_retries++;
      if(num_retries>(1000000/50000)*5){
	post("snd0_unbind: Waited 5 seconds for pd. Something is probably wrong. Could not unbind.");
	RU_;
      }
      usleep(50000);
    }
  }

  scm_unprotect_object(func);

  RU_;
}







/*****************************************************************************************************
 *****************************************************************************************************
 *    Got data from the guile side. Distributing to outlets or receivers.
 *    The guile side is responsible for checking that the arguments are correct.
 *****************************************************************************************************
 *****************************************************************************************************/

#define GET_CLASS() (INTEGER_P(symbol)?(t_symbol*)GET_POINTER(symbol):MAKE_SYM(symbol))->s_thing
#define CLASS_INIT t_class **s=GET_CLASS();if(s==NULL) post("no receiver"); else 
#define GET_OUTLET() GET_X(instance)->outlets[GET_INTEGER(outlet)]


/* Number -> float */
static void pd0_outlet_number(struct dispatch *d){
  outlet_float(d->x->outlets[d->data.outlet_num],d->data2.val);
}

static SCM snd0_outlet_number(SCM instance,SCM outlet,SCM val){
  struct dispatch d;
  d.func=pd0_outlet_number;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  d.data2.val=scm_num2dbl(val,"snd0_outlet_number:");
  snd0_send_message(&d);
  RU_;
}


static void pd0_send_number(struct dispatch *d){
  pd_float(d->data.class,d->data2.val);
}
static SCM snd0_send_number(SCM symbol,SCM val){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_number;
      d.data.class=s;
      d.data2.val=scm_num2dbl(val,"snd0_send_number:");
      snd0_send_message(&d);
    }
  RU_;
}


static t_atom snd_pd_atom[5000]; // Should be huge enough.
static volatile int snd_pd_atom_in_use=0; // lock variable for snd_pd_atom.

/* List -> list */
static void snd0_make_list(SCM val){
  t_atom *atom=snd_pd_atom;
  int lokke;
  int length=GET_INTEGER(scm_length(val));

  for(lokke=0;lokke<length;lokke++){
    SCM el=scm_list_ref(val,MAKE_INTEGER(lokke));
    t_atom *to=&atom[lokke];
    if(SCM_INUMP(el)){
      SETFLOAT(to,(float)GET_INTEGER(el));
    }else{
      if(SCM_UNBNDP(el)){
	SETSYMBOL(to,snd0_gensym("undefined"));
      }else{
	if(SCM_STRINGP(el)){
	  SETSYMBOL(to,snd0_gensym(SCM_STRING_CHARS(el)));
	}else{
	  if(SCM_SYMBOLP(el)){
	    SETSYMBOL(to,MAKE_SYM(el));
	  }else{
	    if(scm_number_p(el)){
	      if(scm_real_p(el)){
		SETFLOAT(to,(float)scm_num2dbl(el,"snd0_outlet_or_send_list"));
	      }else{
		post("Illegal argument to gdp_outlet_or_send_list. Setting atom to 0.");
		SETFLOAT(to,0.0f);
	      }
	    }
	  }
	}
      }
    }
  }
}
static void pd0_outlet_list(struct dispatch *d){
  outlet_list(d->x->outlets[d->data.outlet_num], &s_list, d->data2.list_length, snd_pd_atom);
  snd_pd_atom_in_use=0;
}
static SCM snd0_outlet_list(SCM instance,SCM outlet,SCM val){
  if(snd_pd_atom_in_use==1){
    int num_retries=0;
    scm_gc();
    while(snd_pd_atom_in_use==1){
      num_retries++;
      if(num_retries>(1000000/50000)*5){
	post("snd0_outlet_list: Waited 5 seconds for pd to process a list. Something is probably wrong. Could not send list to outlet.");
	RU_;
      }
      usleep(50000);
    }
  }
  snd_pd_atom_in_use=1;
  {
    struct dispatch d;
    d.func=pd0_outlet_list;
    d.x=GET_X(instance);
    d.data.outlet_num=GET_INTEGER(outlet);
    d.data2.list_length=GET_INTEGER(scm_length(val));
    snd0_make_list(val);
    snd0_send_message(&d);
  }
  RU_;
}
static void pd0_send_list(struct dispatch *d){
  pd_list(d->data.class, &s_list, d->data2.list_length, snd_pd_atom);
  snd_pd_atom_in_use=0;
}
static SCM snd0_send_list(SCM symbol,SCM val){
  if(snd_pd_atom_in_use==1){
    int num_retries=0;
    scm_gc();
    while(snd_pd_atom_in_use==1){
      num_retries++;
      if(num_retries>(1000000/50000)*5){
	post("snd0_send_list: Waited 5 seconds for pd to process a list. Something is probably wrong. Could not send list to receiver.");
	RU_;
      }
      usleep(50000);
    }
  }
  snd_pd_atom_in_use=1;
  {
    CLASS_INIT
      {
	struct dispatch d;
	d.func=pd0_send_list;
	d.data.class=s;
	d.data2.list_length=GET_INTEGER(scm_length(val));
	snd0_make_list(val);
	snd0_send_message(&d);
      }
  }
  RU_;
}

/* Symbol -> symbol */
static void pd0_outlet_symbol(struct dispatch *d){
  outlet_symbol(d->x->outlets[d->data.outlet_num],d->data2.symbol);
}
static SCM snd0_outlet_symbol(SCM instance,SCM outlet,SCM val){
  struct dispatch d;
  d.func=pd0_outlet_symbol;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  d.data2.symbol=MAKE_SYM(val);
  snd0_send_message(&d);
  RU_;
}
static void pd0_send_symbol(struct dispatch *d){
  pd_symbol(d->data.class,d->data2.symbol);
}
static SCM snd0_send_symbol(SCM symbol,SCM val){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_symbol;
      d.data.class=s;
      d.data2.symbol=MAKE_SYM(val);
      snd0_send_message(&d);
    }
  RU_;
}

/* String -> symbol */
static SCM snd0_outlet_string(SCM instance,SCM outlet,SCM val){
  struct dispatch d;
  d.func=pd0_outlet_symbol;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  d.data2.symbol=snd0_gensym(SCM_STRING_CHARS(val));
  snd0_send_message(&d);
  RU_;
}
static SCM snd0_send_string(SCM symbol,SCM val){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_symbol;
      d.data.class=s;
      d.data2.symbol=snd0_gensym(SCM_STRING_CHARS(val));
      snd0_send_message(&d);
    }
  RU_;
}

/* Bang -> bang */
static void pd0_outlet_bang(struct dispatch *d){
  outlet_bang(d->x->outlets[d->data.outlet_num]);
}
static SCM snd0_outlet_bang(SCM instance,SCM outlet){
  struct dispatch d;
  d.func=pd0_outlet_bang;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  snd0_send_message(&d);
  RU_;
}
static void pd0_send_bang(struct dispatch *d){
  pd_bang(d->data.class);
}
static SCM snd0_send_bang(SCM symbol){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_bang;
      d.data.class=s;
      snd0_send_message(&d);
    }
  RU_;
}

/* <- symbol */
static SCM snd0_get_symbol(SCM symname){
  return MAKE_POINTER(MAKE_SYM(symname));
}



/*****************************************************************************************************
 *****************************************************************************************************
 *    Setting up global guile functions.
 *****************************************************************************************************
 *****************************************************************************************************/


static void *snd0_init_in_guile(void *arg){
  snd_pd_main();
  XEN_YES_WE_HAVE("snd-pd-external");

  scm_c_define_gsubr("pd-c-bind",2,0,0,snd0_bind);
  scm_c_define_gsubr("pd-c-unbind",2,0,0,snd0_unbind);
  scm_c_define_gsubr("pd-c-outlet-number",3,0,0,snd0_outlet_number);
  scm_c_define_gsubr("pd-c-outlet-list",3,0,0,snd0_outlet_list);
  scm_c_define_gsubr("pd-c-outlet-symbol",3,0,0,snd0_outlet_symbol);
  scm_c_define_gsubr("pd-c-outlet-string",3,0,0,snd0_outlet_string);
  scm_c_define_gsubr("pd-c-outlet-bang",2,0,0,snd0_outlet_bang);
  scm_c_define_gsubr("pd-c-send-number",2,0,0,snd0_send_number);
  scm_c_define_gsubr("pd-c-send-list",2,0,0,snd0_send_list);
  scm_c_define_gsubr("pd-c-send-symbol",2,0,0,snd0_send_symbol);
  scm_c_define_gsubr("pd-c-send-string",2,0,0,snd0_send_string);
  scm_c_define_gsubr("pd-c-send-bang",1,0,0,snd0_send_bang);
  scm_c_define_gsubr("pd-c-get-symbol",1,0,0,snd0_get_symbol);

  
  EVAL("(set! %load-path (cons \"" SND_PD_PATH "\" %load-path))"
       "(if (not (provided? 'snd-pd-global.scm)) (load-from-path \"pd-global.scm\"))");
  
  eval_string_func=EVAL("eval-string");

  if(pthread_create(&pthread_repl,NULL,read_eval_print_loop,NULL)!=0){
    post("Could not make pthread. (disaster!)\n");
  }  

  snd0_das_dispatcher();

  return NULL;
}

static void *snd0_init(void *arg){
  return scm_with_guile(snd0_init_in_guile,arg);
}

static void pd0_init(void){
  rb_snd_to_pd = jack_ringbuffer_create(sizeof(struct dispatch)*1024);
  rb_pd_to_snd = jack_ringbuffer_create(sizeof(struct dispatch)*1024);
  rb_anything  = jack_ringbuffer_create(1024*64);
  rb_repl  = jack_ringbuffer_create(504);

  snd_pd_clock = clock_new(NULL,(t_method)pd0_das_dispatcher);
  clock_delay(snd_pd_clock, 1.0);

  //scm_init_guile();

  if(pthread_create(&pthread,NULL,snd0_init,NULL)!=0){
    post("Could not make pthread. (disaster!)\n");
  }  

  return;
}





/*****************************************************************************************************
 *****************************************************************************************************
 *    DSP
 *****************************************************************************************************
 *****************************************************************************************************/

typedef void (*PD_RT_PROCESS)(int num_ins,float **ins,int num_outs,float **outs,void *inbus,void *outbus,int nframes);
static PD_RT_PROCESS pd_rt_process=NULL;
typedef void (*PD_RT_RUN)(int nframes);
static PD_RT_RUN pd_rt_run=NULL;

extern void snd_pd_set_rt_funcs(PD_RT_RUN r,PD_RT_PROCESS p){
  pd_rt_run=r;
  pd_rt_process=p;
}

static t_int *snd_pd_perform(t_int *w){
  t_snd_pd *x=(t_snd_pd*)w[1];
  int ch,lokke;
  int length=(int)w[2];
  t_float **ins;
  t_float **outs;
  static double last_time=0.0;

  ins=(t_float**)&w[3];
  outs=(t_float**)&w[3+x->num_ins];

  if(x->isworking==true){
    if(pd_rt_run!=NULL  && (0 || last_time!=clock_getlogicaltime())){
      last_time=clock_getlogicaltime();
      pd_rt_run(length);
    }

    if(pd_rt_process!=NULL){
      pd_rt_process(x->num_ins,ins,x->num_outs,outs,x->inbus,x->outbus,length);
    } 
  }else{
    for(ch=0;ch<x->num_outs;ch++){
      memset(outs[ch],0,sizeof(t_float)*length);
    }
  }

  return w+x->num_ins+x->num_outs+3;
}

static void snd_pd_dsp(t_snd_pd *x, t_signal **sp)
{
  t_int vec[x->num_ins+x->num_outs+2];
  int lokke=0;
  vec[0]=(int)x;
  vec[1]=(int)sp[0]->s_n;

  int i;
  for (i = 2; i < x->num_ins+x->num_outs+2; i++) {
    vec[i] = (t_int)(sp[i - 1]->s_vec);
  }

  dsp_addv(snd_pd_perform,
	   x->num_ins+x->num_outs+2,
	   vec);
}




/*****************************************************************************************************
 *****************************************************************************************************
 *    Starting and stopping new guile script
 *****************************************************************************************************
 *****************************************************************************************************/

static void snd0_load(struct dispatch *d){
  SCM evalret;
  bool ret=false;
  char code[500];
  char errormessage[500];

  FILE *file=fopen(d->data.filename,"r");
  if(file==NULL){
    post("file \"%s\" not found.\n",d->data.filename);
    goto exit;
  }
  fclose(file);


  // Let the file live in its own name-space (or something like that).
  snd0_eval2("(define (pd-instance-func pd-instance pd-num-inlets pd-num-outlets) (fix-defines ");
  snd0_eval_file(SND_PD_PATH "/pd-local.scm");
  snd0_eval_file(d->data.filename);
  snd0_eval2("  (list pd-inlet-func pd-cleanup-func (if (defined? '*rt-engine*)"
	                                               "*in-bus* 0)"
	                                           "(if (defined? '*rt-engine*)"
	                                               "*out-bus* 0))))");
  //  snd0_eval2("  (list pd-inlet-func pd-cleanup-func (if (defined? '*rt-engine*)"
  //	                                               "(SCM_SMOB_DATA *in-bus*) (list \"POINTER\" 0))"
  //	                                           "(if (defined? '*rt-engine*)"
  //	                                               "(SCM_SMOB_DATA *out-bus*) (list \"POINTER\" 0))))");
  snd0_eval2("1");

  if(1!=GET_INTEGER(snd0_eval_do())){
    post("Failed.");
    goto exit;
  }

  sprintf(code,"(pd-instance-func %d %d %d)",(int)d->x,d->x->num_ins,d->x->num_outs);
  sprintf(errormessage,"When loading file \"%s\"\n",d->data.filename);
  evalret=CATCH_EVAL(code,errormessage);  

  if(!SCM_CONSP(evalret)){
    post("Failed.");
    goto exit;
  }
  d->x->inlet_func=SCM_CAR(evalret);
  d->x->cleanup_func=SCM_CAR(SCM_CDR(evalret));
  d->x->scm_inbus=SCM_CAR(SCM_CDR(SCM_CDR(evalret)));
  d->x->scm_outbus=SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(evalret))));

  scm_gc_protect_object(d->x->inlet_func);
  scm_gc_protect_object(d->x->cleanup_func);
  scm_gc_protect_object(d->x->scm_inbus);
  scm_gc_protect_object(d->x->scm_outbus);

  d->x->inbus=(void*)SCM_SMOB_DATA(d->x->scm_inbus);
  d->x->outbus=(void*)SCM_SMOB_DATA(d->x->scm_outbus);

  ret=true;
  post("\"%s\" loaded by snd.",d->data.filename);

 exit:
  d->x->isworking=ret;
  
  //  return ret;
  return;
}

static bool pd0_load(t_snd_pd *x,char *filename){
  struct dispatch d;
  d.func=snd0_load;
  d.x=x;
  d.data.filename=filename;
  pd0_send_message(&d);
  return true;
}

static void *pd0_new(t_symbol *s, t_int argc, t_atom* argv){
  int lokke;
  t_snd_pd *x;

  if(argc==0){
    post("Usage: snd [filename] <num_inlets> <num_outlets>");
    return NULL;
  }

  x= (t_snd_pd *)pd_new(snd_pd_class);
  //x->isinited=false;
  x->filename=atom_getsymbolarg(0,argc,argv)->s_name;


  /************** inlets ****************/
  x->num_ins=1;
  if(argc>1)
    x->num_ins=atom_getfloatarg(1,argc,argv);
  
  x->inlets=calloc(sizeof(t_snd_pd_workaround*),x->num_ins);
  for(lokke=1;lokke<x->num_ins;lokke++){
    t_snd_pd_workaround *x2;
    x2=(t_snd_pd_workaround*)pd_new(snd_pd_workaroundclass);
    x->inlets[lokke]=x2;
    x2->x=x;
    x2->index=lokke;
    x2->inlet=inlet_new(&x->x_obj,(t_pd*)x2,0,0);
    //x2->inlet=inlet_new(&x->x_obj,(t_pd*)x2,&s_signal,&s_signal);
    //inlet_new(&x->x_obj, &x->x_obj.ob_pd, &s_signal, &s_signal);
  }
  for(lokke=0;lokke<x->num_ins;lokke++){
    inlet_new(&x->x_obj, &x->x_obj.ob_pd, &s_signal, &s_signal);
  }

  /************* outlets ****************/
  x->num_outs=1;
  if(argc>2)
    x->num_outs=atom_getfloatarg(2,argc,argv);
  x->outlets=calloc(sizeof(t_outlet*),x->num_outs);


  for(lokke=0;lokke<x->num_outs;lokke++){
    x->outlets[lokke] = outlet_new(&x->x_obj, gensym("anything"));
  }

  for(lokke=0;lokke<x->num_outs;lokke++){
    outlet_new(&x->x_obj, gensym("signal"));
  }



  if(pd0_load(x,x->filename)==true){
    //x->isinited=true;
    return x;
  }

  return NULL;
}

static void snd0_free(struct dispatch *d){
  if(d->x->isworking==true){
    g_call0(d->x->cleanup_func,"cleanup func.\n");
    scm_gc_unprotect_object(d->x->inlet_func);
    scm_gc_unprotect_object(d->x->cleanup_func);
    scm_gc_unprotect_object(d->x->scm_inbus);
    scm_gc_unprotect_object(d->x->scm_outbus);
  }

  free(d->x->inlets);
  free(d->x->outlets);
  free(d->x);
}

static void pd0_free(t_snd_pd *x){
  int lokke;
  struct dispatch d;
  t_snd_pd *new_x=malloc(sizeof(t_snd_pd));

  memcpy(new_x,x,sizeof(t_snd_pd));
  d.func=snd0_free;
  d.x=new_x;

  for(lokke=1;lokke<x->num_ins;lokke++){
    inlet_free(x->inlets[lokke]->inlet);
    pd_free((t_pd*)x->inlets[lokke]);
  }
  for(lokke=0;lokke<x->num_outs;lokke++){
    outlet_free(x->outlets[lokke]);
  }

  pd0_send_message(&d);
  
}


static void snd0_reload(struct dispatch *d){
  //scm_call_1(pd_backtrace_run,d->x->cleanup_func);
  if(d->x->isworking==true){
    g_call0(d->x->cleanup_func,"in snd0_reload\n");
    scm_gc_unprotect_object(d->x->inlet_func);
    scm_gc_unprotect_object(d->x->cleanup_func);
  }
  snd0_load(d);
}

static void pd0_reload(t_snd_pd *x){
  struct dispatch d;
  d.func=snd0_reload;
  d.x=x;
  d.data.filename=x->filename;
  pd0_send_message(&d);
}

static void snd0_eval(struct dispatch *d){
  //scm_call_2(pd_backtrace_run1,eval_string_func,MAKE_STRING(d->data2.symbol->s_name));
  g_call1(eval_string_func,MAKE_STRING(d->data2.symbol->s_name),"in snd0_eval\n");
}

static void pd0_eval(t_snd_pd *x,t_symbol *s){
  struct dispatch d;
  d.func=snd0_eval;
  d.data2.symbol=s;
  pd0_send_message(&d);
}

//static void snd_pd_evalfile(t_snd_pd *x,t_symbol *s){
//}



static void pd0_at_exit(void){
  //fprintf(stderr,"upp\n");
}

static void finish(int sig){
  pd0_at_exit();
  exit(0);
}
static void sigusr2callback(int sig){
  fprintf(stderr,"%s/%d: pd just got the SIGUSR2 signal. This might that a sched_fifo thread is sleeping or something.\n", __FILE__,__LINE__);
  fprintf(stderr,"(don't bother reporting this, unless you hear clicks at the same time, its not an error.)\n");
}


/*****************************************************************************************************
 *****************************************************************************************************
 *    Das setup
 *****************************************************************************************************
 *****************************************************************************************************/
void snd_setup(void){ // (pd0_setup)

  if(0==SCM_USE_PTHREAD_THREADS){
    post("Error. Snd/Pd needs to be built against Guile configured with \"--with-threads\". (see manual)");
    return;
  }

  pd0_init();

  snd_pd_class = class_new(gensym("snd"), (t_newmethod)pd0_new,
			   (t_method)pd0_free, sizeof(t_snd_pd), 0, A_GIMME, 0);

  class_addanything(snd_pd_class, (t_method)pd0_anything_first);
  class_addmethod(snd_pd_class, (t_method)pd0_reload, gensym("reload"), 0);
  class_addmethod(snd_pd_class, (t_method)pd0_eval, gensym("eval"), A_DEFSYM,0);
  //class_addmethod(snd_pd_class, (t_method)snd_pd_evalfile, gensym("evalfile"), A_DEFSYM,0);
  class_sethelpsymbol(snd_pd_class, gensym("help-snd_pd.pd"));


  /* This trick(?) is taken from the flext source. (I don't understand what happens...) */
  snd_pd_workaroundclass=class_new(gensym("indexworkaround"),NULL,NULL,sizeof(t_snd_pd_workaround),CLASS_PD|CLASS_NOINLET, A_NULL);
  class_addanything(snd_pd_workaroundclass,pd0_anything);

  //	    class_domainsignalin(snd_pd_class, -1);
  //	    class_addfloat(snd_pd_class, 0);

  //CLASS_MAINSIGNALIN(snd_pd_class, t_snd_pd, x_float);
  class_addmethod(snd_pd_class, (t_method)snd_pd_dsp, gensym("dsp"), 0);


  /* Following 6 lines taken from the plugin external source. -Kjetil. */
  /* We have to make a "null" callback for signal input to the first
     inlet or otherwise Pd'll gracefully fuck the inlets up */
  class_addmethod (snd_pd_class,
		   nullfn,
		   gensym ("signal"),
		   0);

  atexit(pd0_at_exit);
  signal(SIGINT,finish);
  signal(SIGUSR2,sigusr2callback);
  post(version);
}


