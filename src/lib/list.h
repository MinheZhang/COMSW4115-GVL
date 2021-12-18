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

typedef list_node * list_iterator;

typedef struct list_t {
    list_node *first;
    list_node *last;
    // size_t data_size;
} list;

list *create_list();
int is_empty(list *l);
int insert_front(list *l, void *data);
int insert_back(list *l, void *data);
void *remove_front(list *l);
void *remove_back(list *l);
void *find(int (*cmp)(const void *, const void *), void *ref, list *l);
void *remove_list_node(int (*cmp)(const void *, const void *), void *ref, list *l);
void remove_all(list *l);
int destroy_list(list *l);
// list copy_all(list *l);

/******************************* iterator *******************************/
list_iterator list_begin(list *l);
list_iterator list_end();
list_iterator list_iter_next(list_iterator iter);
void *list_iter_data(list_iterator iter);

#endif //GRAPH_LIST_H
