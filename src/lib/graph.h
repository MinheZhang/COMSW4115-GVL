//
// Created by cyx on 2021/11/29.
//

#ifndef GRAPH_GRAPH_H
#define GRAPH_GRAPH_H

#include "list.h"

typedef struct node_t {
    float x;
    float y;
    float radius;
    int r;
    int g;
    int b;
} node;

typedef struct edge_t {
    node *start;
    node *end;
    int bold;
    int r;
    int g;
    int b;
} edge;

typedef struct graph_node_t {
    node n;
    list edges;  // list of edge
} graph_node;

typedef list graph;

// graph: list of graph_node
// the node pointer in edge points to the address of node in graph_node?

#endif //GRAPH_GRAPH_H
