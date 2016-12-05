#include <stdio.h>
#include <stdlib.h>

/*

  let f y =
     let z = 2 * y in
     fun x -> x + z

 */
typedef int (*fun_t) (int);

fun_t f (int y) {
  int z = 2 * y;
  int g (int x) {
    return (z + x);
  }
  return g;
}

void o (int x, int y, int z) {
  x = 42;
  y = 33;
  z = 69;
  printf ("%d %d %d\n", x, y, z);
}

int main (char** argv) {
  fun_t h = f (5);
  o (33, 22, 22);
  int u = h (2);
  printf ("u = %d\n", u);
  int v = h (3);
  printf ("v = %d\n", v);
  exit (EXIT_SUCCESS);
}
