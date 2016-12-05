#include <stdio.h>
#include <stdlib.h>

void iter (void (*f)(int), int src[], unsigned int count)
{
   unsigned int i;
   for (i = 0; i < count; i++)
     f (src[i]);
}

typedef int (*int_int_fun_t) (int);

void map (int_int_fun_t f, int src[], int dst[], unsigned int count)
{
   unsigned int i;
   for (i = 0; i < count; i++)
     dst[i] = f (src[i]);
}

int twice (int x) {
  return (x + x);
}

void print (int x) {
  printf ("%d\n", x);
}

int_int_fun_t add1 (int x) {
  int add2 (int y) { return (x + y); }
  return add2;
}

#define size 5

int main (int argc, char** argv) {
  int data[size] = { 1, 2, 3, 4, 5 };
  map (add1 (10), data, data, size);
  iter (print, data, size);
  exit (EXIT_SUCCESS);
}
