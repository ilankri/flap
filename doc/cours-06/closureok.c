#include <stdio.h>
#include <stdlib.h>

typedef int* env_t;

typedef int (*int_int_closed_fun_t) (env_t, int);

int lookup_env (env_t env, int key) {
  return (env[key]);
}

env_t mk_env1 (int v) {
  env_t k = (env_t) malloc (sizeof (int));
  k[0] = v;
  return (k);
}

typedef struct closure {
  env_t env;
  int_int_closed_fun_t code;
} closure_t;

closure_t* mk_closure (env_t env, int_int_closed_fun_t code) {
  closure_t* closure = (closure_t*) malloc (sizeof (closure));
  closure->env  = env;
  closure->code = code;
  return (closure);
}

// f (x)
int int_apply (closure_t* f, int x) {
  return (f->code (f->env, x));
}

void map (closure_t* f, int src[], int dst[], unsigned int count)
{
   unsigned int i;
   for (i = 0; i < count; i++)
     dst[i] = int_apply (f, src[i]);
}

int add2 (env_t env, int y) {
  return (lookup_env (env, 0) + y);
}

// let add1 x = fun y -> x + y
closure_t* add1 (int x) {
  return (mk_closure (mk_env1 (x), add2));
}

#define size 5

int main (int argc, char** argv) {
  int data[size] = { 1, 2, 3, 4, 5 };
  map (add1 (10), data, data, size);
  printf ("%d\n", data[0]);
  exit (EXIT_SUCCESS);
}
