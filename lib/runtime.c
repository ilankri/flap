#include <stdio.h>
#include <stdlib.h>

void print_int(int i)
{
	printf("%d", i);
}

void print_string(char *s)
{
	printf("%s", s);
}

int *block_create(int size)
{
	return malloc(sizeof(int) * size);
}

int block_get(int *block, int index)
{
	return block[index];
}

void block_set(int *block, int index, int value)
{
	block[index] = value;
}
