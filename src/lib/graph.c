//
// Created by cyx on 2021/11/29.
//

#include <memory.h>
#include <stdio.h>
#include "graph.h"

/******************************* utils *******************************/

int cmp_node(const void *n, const void *ref) {
    graph_node *gn = (graph_node *)n;
    graph_node *gref = (graph_node *)ref;
    // return (gn->n == gref->n);
    return (gn->n->x == gref->n->x && gn->n->y == gref->n->y && gn->n->radius == gref->n->radius &&
            gn->n->r == gref->n->r && gn->n->g == gref->n->g && gn->n->b == gref->n->b
            && gn->n->extra == gref->n->extra);
}

int cmp_edge(const void *e, const void *ref) {
    edge *ee = (edge *)e;
    edge *eref = (edge *)ref;
    return (ee->start == eref->start && ee->end == eref->end && ee->bold == eref->bold &&
            ee->r == eref->r && ee->g == eref->g && ee->b == eref->b);
}

int cmp_edge_end_node(const void *e, const void *ref) {
    edge *ee = (edge *)e;
    edge *eref = (edge *)ref;
    return (ee->end == eref->end);
}

void print_graph(graph *g, int simple) {
    list_node *iter = g->first;
    while(iter) {
        graph_node *gn = (graph_node *)iter->data;
        list_node *edge_iter = gn->edges->first;

        if (simple) {
            printf("(%g, %g)\n", gn->n->x, gn->n->y);
        } else {
            printf("(x:%g, y%g, radius:%g, r:%d, g:%d, b:%d)\n",
                    gn->n->x, gn->n->y, gn->n->radius, gn->n->r, gn->n->g, gn->n->b);
        }

        while (edge_iter) {
            edge *e = (edge *)edge_iter->data;
            printf("(%g, %g)", e->end->x, e->end->y);
            edge_iter = edge_iter->next;
        }
        printf("\n");
        iter = iter->next;
    }
}

/******************************* node *******************************/

node *create_node(double x, double y, double radius, int r, int g, int b, void *extra) {
    node *n = malloc(sizeof(node));
    n->x = x;
    n->y = y;
    n->radius = radius;
    n->r = r;
    n->g = g;
    n->b = b;
    n->extra = extra;
    return n;
}

int set_node_color(node *n, int r, int g, int b) {
    n->r = r;
    n->g = g;
    n->b = b;
    return 0;
}

double get_node_x(node *n) {
    return n->x;
}

double get_node_y(node *n) {
    return n->y;
}

double get_node_radius(node *n) {
    return n->radius;
}

int get_node_r(node *n) {
    return n->r;
}

int get_node_g(node *n) {
    return n->g;
}

int get_node_b(node *n) {
    return n->b;
}

void *get_node_extra(node *n) {
    return n->extra;
}

int set_node_x(node *n, double x) {
    n->x = x;
    return 0;
}

int set_node_y(node *n, double y) {
    n->y = y;
    return 0;
}

int set_node_radius(node *n, double radius) {
    n->radius = radius;
    return 0;
}

int set_node_r(node *n, int r) {
    n->r = r;
    return 0;
}

int set_node_g(node *n, int g) {
    n->g = g;
    return 0;
}

int set_node_b(node *n, int b) {
    n->b = b;    
    return 0;
}

int set_node_extra(node *n, void *extra) {
    n->extra = extra;
    return 0;
}

/******************************* edge *******************************/

edge *create_edge(node *start, node *end, int bold, int r, int g, int b) {
    edge *e = malloc(sizeof(edge));
    e->start = start;
    e->end = end;
    e->bold = bold;
    e->r = r;
    e->g = g;
    e->b = b;
    return e;
}

int edge_change_color(edge *e, int r, int g, int b) {
    e->r = r;
    e->g = g;
    e->b = b;
    return 0;
}

// Get edge attributes.
node *get_edge_start(edge *e) {
    return e->start;
}

node *get_edge_end(edge *e) {
    return e->end;
}

int get_edge_bold(edge *e) {
    return e->bold;
}

int get_edge_r(edge *e) {
    return e->r;
}

int get_edge_g(edge *e) {
    return e->g;
}

int get_edge_b(edge *e) {
    return e->b;
}

// Set edge attributes.
int set_edge_start(edge *e, node *start) {
    e->start = start;
    return 0;
}

int set_edge_end(edge *e, node *end) {
    e->end = end;
    return 0;
}

int set_edge_bold(edge *e, int bold) {
    e->bold = bold;
    return 0;
}

int set_edge_r(edge *e, int r) {
    e->r = r;
    return 0;
}

int set_edge_g(edge *e, int g) {
    e->g = g;
    return 0;
}

int set_edge_b(edge *e, int b) {
    e->b = b;
    return 0;
}

/******************************* graph *******************************/

graph *create_graph() {
    graph *g;
    g = create_list(sizeof(graph_node));
    return g;
}

int add_node(graph *g, node *n) {
    graph_node *gn = malloc(sizeof(graph_node));
    gn->n = n;
    gn->edges = create_list(sizeof(edge));
    insert_front(g, gn);
    return 0;
}

int remove_node(graph *g, node *n) {
    graph_node gn_ref;
    gn_ref.n = n;

    // remove all edges containing this node
    list_node *iter = g->first;
    while(iter) {
        graph_node *gn = (graph_node *)iter->data;

        edge e_ref;
        e_ref.end = n;
        edge *removed_e = remove_list_node(cmp_edge_end_node, &e_ref, gn->edges);
        free(removed_e);

        iter = iter->next;
    }

    // remove node from graph
    graph_node *data = (graph_node *)remove_list_node(cmp_node, &gn_ref, g);
    free(data->n->extra); // free extra
    free(data->n);
    remove_all(data->edges);
    free(data);

    return 0;
}

int add_edge(graph *g, edge *e) {
    graph_node start_ref;
    start_ref.n = e->start;
    graph_node *start_gn = find(cmp_node, &start_ref, g);
    if (start_gn == NULL) {
        return 1;
    }
    insert_back(start_gn->edges, e);
    return 0;
}

int remove_edge(graph *g, edge *e) {
    graph_node start_ref;
    start_ref.n = e->start;
    graph_node *start_gn = find(cmp_node, &start_ref, g);
    edge *removed_e = remove_list_node(cmp_edge, e, start_gn->edges);
    free(removed_e);
    return 0;
}

int destroy_graph(graph *g) {
    while (!is_empty(g)) {
        graph_node *gn = (graph_node *)remove_front(g);
        remove_all(gn->edges);
        free(gn->n->extra); // free extra
        free(gn->n);
        free(gn);
    }
    free(g);
    return 0;
}

list *get_edges(graph *g, node *n) {
    graph_node ref;
    ref.n = n;
    graph_node *gn = find(cmp_node, &ref, g);

    return gn->edges;
}
