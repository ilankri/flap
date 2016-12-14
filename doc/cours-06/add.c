#include <stdio.h>
#include <stdlib.h>

/*

  let f y =
     let z = 2 * y in
     fun x -> x + z

 */
typedef int (*fun_t) (int);

fun_t f (int y) {
  int g (int x) {
    return (2 / y + x);
  }
  return g;
}

void o (int x, int y, int z) {
  x = 0;
  y = 0;
  z = 0;
  printf ("%d %d %d\n", x, y, z);
}

int main (char** argv) {
  fun_t h = f (5);
  o (0, 0, 0);
  int u = h (2);
  printf ("u = %d\n", u);
  int v = h (3);
  printf ("v = %d\n", v);
  exit (EXIT_SUCCESS);
}
