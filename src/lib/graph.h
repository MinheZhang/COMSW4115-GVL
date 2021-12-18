//
// Created by cyx on 2021/11/29.
//

#ifndef GRAPH_GRAPH_H
#define GRAPH_GRAPH_H

#include "list.h"

typedef struct node_t {
    double x;
    double y;
    double radius;
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
    list *edges;  // list of edge (data is pointer to edge)
} graph_node;

typedef list graph;  // graph: list of graph_node
// the node pointer in edge points to the address of node in graph_node

/******************************* Node *******************************/

node *create_node(double x, double y, double radius, int r, int g, int b, void *data);

// Get node attributes.
double get_node_x(node *n);
double get_node_y(node *n);
double get_node_radius(node *n);
int get_node_r(node *n);
int get_node_g(node *n);
int get_node_b(node *n);
void *get_node_extra(node *n);

// Set node attributes.
int set_node_x(node *n, double x);
int set_node_y(node *n, double y);
int set_node_radius(node *n, double radius);
int set_node_r(node *n, int r);
int set_node_g(node *n, int g);
int set_node_b(node *n, int b);
int set_node_extra(node *n, void *extra);
int set_node_color(node *n, int r, int g, int b);

/******************************* Edge *******************************/

edge *create_edge(node *start, node *end, int bold, int r, int g, int b);
int edge_change_color(edge *e, int r, int g, int b);

// Get edge attributes.
node *get_edge_start(edge *e);
node *get_edge_end(edge *e);
int get_edge_bold(edge *e);
int get_edge_r(edge *e);
int get_edge_g(edge *e);
int get_edge_b(edge *e);
int destroy_node(node *n);

// Set edge attributes.
int set_edge_start(edge *e, node *start);
int set_edge_end(edge *e, node *end);
int set_edge_bold(edge *e, int bold);
int set_edge_r(edge *e, int r);
int set_edge_g(edge *e, int g);
int set_edge_b(edge *e, int b);
int destroy_edge(edge *e);

/******************************* Graph *******************************/

graph *create_graph();
int add_node(graph *g, node *n);
int remove_node(graph *g, node *n);
int add_edge(graph *g, edge *e);
int remove_edge(graph *g, edge *e);
int destroy_graph(graph *g);

list *get_edges(graph *g, node *n);

void print_graph(graph *g, int simple);

/******************************* Visualization *******************************/
int show_graph(graph *g);

#endif //GRAPH_GRAPH_H
