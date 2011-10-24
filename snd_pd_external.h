
/* --------------------------- snd_pd_external  ------------------------------- */
/* Made by Kjetil S. Matheussen, 2005/2006.                                          */

struct snd_pd_workaround;

typedef struct snd_pd
{
  t_object x_obj;

  int num_ins;
  int num_outs;

  struct snd_pd_workaround **inlets;
  t_outlet **outlets;

  SCM inlet_func;
  SCM cleanup_func;

  void* inbus;
  void* outbus;

  char *filename;

  bool isworking;

  float x_float;

  // Keep pointer to these to be able to unprotect from the garbage collector when freeing the object.
  SCM scm_inbus;
  SCM scm_outbus;

} t_snd_pd;

typedef struct snd_pd_workaround{
  t_object x_obj;
  t_snd_pd *x;
  t_inlet *inlet;
  int index;
  SCM func;
} t_snd_pd_workaround;




#define KG_MAX(a,b) (((a)>(b))?(a):(b))
#define KG_MIN(a,b) (((a)<(b))?(a):(b))


