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
    void *extra;
    // TODO in codegen, turn all added attributes (extra) into pointer_type of struct?
    // or use the change of color to indicate whether it is visited
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
    node *n;
    list edges;  // list of edge (data is pointer to edge)
} graph_node;

typedef list graph;  // graph: list of graph_node
// the node pointer in edge points to the address of node in graph_node

node *create_node(float x, float y, float radius, int r, int g, int b, void *data);
void *node_get_extra(node *n);
int node_change_color(node *n, int r, int g, int b);

edge *create_edge(node *start, node *end, int bold, int r, int g, int b);
node *edge_get_start(edge *e);
node *edge_get_end(edge *e);
int edge_change_color(edge *e, int r, int g, int b);

graph *create_graph();
int add_node(graph *g, node *n);
int remove_node(graph *g, node *n);
int add_edge(graph *g, edge *e);
int remove_edge(graph *g, edge *e);
int destroy_graph(graph *g);

list get_edges(graph *g, node *n);

void print_graph(graph *g, int simple);

#endif //GRAPH_GRAPH_H
