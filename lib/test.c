#include <stdio.h>
#include <stdlib.h>
//For execution : gcc -c runtime.c; gcc -o runetime runtime.o
void print_int(int n1, int n2, int n3, int n4, int n5)
{
  printf("Int: %d-%d-%d-%d-%d\n", n1, n2, n3, n4, n5);
}

//For unit test
int main (int argc, char* argv[])
{
  print_int(11, 22, 33, 44, 55);
  return 0;
}
