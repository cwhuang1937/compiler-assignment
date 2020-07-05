#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct symbol_entry {
    int idx;
    char name[100];
    char typ[10];
    int addr;
    int line;
    char ele_typ[10];
    struct symbol_entry* next;
};
struct symbol_table {
    struct symbol_entry* head;
    struct symbol_entry* tail;
    int size;
};

#endif /* COMMON_H */