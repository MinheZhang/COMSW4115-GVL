//
// Created by cyx on 2021/11/29.
//

#ifndef GRAPH_LIST_H
#define GRAPH_LIST_H

#include <stdlib.h>

typedef struct list_node_t {
    void *data;
    struct list_node_t *next;
    struct list_node_t *prev;
} list_node;

typedef struct list_t {
    list_node *first;
    list_node *last;
    size_t data_size;
} list;

list create_list(size_t data_size);
int is_empty(list *l);
void insert_front(void *data, list *l);
void insert_back(void *data, list *l);
void *remove_front(list *l);
void *remove_back(list *l);
void remove_all(list *l);
list copy_all(list *l);

#endif //GRAPH_LIST_H
