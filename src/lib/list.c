//
// Created by cyx on 2021/11/29.
//

#include <memory.h>
#include <stdio.h>
#include "list.h"

list create_list(size_t data_size) {
    list l = {NULL, NULL, data_size};
    return l;
}

int is_empty(list *l) {
    return (l->first == NULL);
}

void insert_front(void *data, list *l) {
    list_node *ln = malloc(sizeof(list_node));
    ln->data = data;
    ln->prev = NULL;
    ln->next = l->first;

    if (l->first != NULL) {
        l->first->prev = ln;
    }
    else {
        l->last = ln;
    }
    l->first = ln;
}

void insert_back(void *data, list *l) {
    list_node *ln = malloc(sizeof(list_node));
    ln->data = data;
    ln->next = NULL;
    ln->prev = l->last;

    if (l->last != NULL) {
        l->last->next = ln;
    }
    else {
        l->first = ln;
    }
    l->last = ln;
}

void *remove_front(list *l) {
    if (is_empty(l)) {
        return NULL;
    }

    list_node *victim = l->first;
    void *data = victim->data;

    l->first = victim->next;
    free(victim);

    if (l->first == NULL) {
        l->last = NULL;
    } else {
        l->first->prev = NULL;
    }

    return data;
}

void *remove_back(list *l) {
    if (is_empty(l)) {
        return NULL;
    }

    list_node *victim = l->last;
    void *data = victim->data;

    l->last = victim->prev;
    free(victim);

    if (l->last == NULL) {
        l->first = NULL;
    } else {
        l->last->next = NULL;
    }

    return data;
}

void remove_all(list *l) {
    while (!is_empty(l)) {
        void *data = remove_front(l);
        free(data);
    }
}

list copy_all(list *l) {
    list new_list = {NULL, NULL, l->data_size};
    list_node *n = l->first;
    while (n) {
        void *data = malloc(l->data_size);
        memcpy(data, n->data, l->data_size);
        insert_back(data, &new_list);
        n = n->next;
    }
    return new_list;
}
