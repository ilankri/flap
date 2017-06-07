#include <stdio.h>
#include <stdlib.h>

void print_int(int n)
{
  printf("%d", n);
}

void print_str(char* str)
{
  printf("%s", str);
}

int* block_create(int size)
{
  return malloc(sizeof(int)*size);
}

int block_get(int* bAddr, int index)
{
  return *(bAddr+index);
}

void block_set(int*bAddr, int index, int value)
{
  bAddr[index] = value;
}

//For self test
/* int main (int argc, char* argv[]) */
/* { */
/*   print_str("TeSt t35T l23"); */
/*   //Simulated retrolix code : */
/*   //  b = block_create(3) */
/*   //  block_set b 0 1    (*for b[0]=1    where 1 is the id of the tag*) */
/*   //  block_set b 1 13   (*for b[1]=13   where 13 is the parameter's value of this tag *) */
/*   //  block_set b 2 0x33 (*for b[2]=0x33 where 0x33 is the address in integer of the other block *) */
/*   int* add2 = block_create(2); */
/*   block_set(add2, 0, 11); */
/*   block_set(add2, 1, 22); */
/*   int v = block_get(add2, 1); */
/*   print_int(v); */
/*   int* add1 = block_create(3); */
/*   block_set(add1, 0, 1); */
/*   block_set(add1, 1, 13); */
/*   block_set(add1, 2, (int)add2); */
/*   print_int((int)add2); */
/*   print_int(block_get(add1, 2)); */
/*   return 0; */
/* } */
