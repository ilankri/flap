#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_int(int i)
{
	printf("%d", i);
}

char *allocate_string(int len)
{
	return malloc(sizeof(char) * (len + 1));
}

void write_string(char *s, int index, char c)
{
	s[index] = c;
}

void print_string(char *s)
{
	printf(s);
}

int equal_string(char *s1, char *s2)
{
	return strcmp(s1, s2) ? 0 : 1;
}

int equal_char(char c1, char c2)
{
	return c1 == c2;
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
