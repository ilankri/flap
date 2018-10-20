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
