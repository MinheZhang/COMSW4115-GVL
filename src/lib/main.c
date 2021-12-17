#include <stdio.h>
#include "list.h"
#include "graph.h"

void dfs(graph *g, node *n) {
    int *visited = get_node_extra(n);
    *visited = 1;

    set_node_color(n, 100, 100, 100);
    printf("%g, %g\n", n->x, n->y);

    list *g_edges = get_edges(g, n);
    for (list_iterator l_iter = list_begin(g_edges); l_iter != list_end(); l_iter = list_iter_next(l_iter)) {
        edge *e = list_iter_data(l_iter);
        node *end_node = get_edge_end(e);
        int *e_visited = get_node_extra(end_node);
        if (!(*e_visited)) {
            dfs(g, end_node);
        }
    }
}

void test_list() {
    list l = create_list(sizeof(int));
    for (int i = 0; i < 10; ++i) {
        int *t = malloc(sizeof(i));
        *t = i;
        insert_back(&l, t);
    }

    for (int i = 0; i < 5; ++i) {
        int *x = (int *)remove_front(&l);
        printf("%d\n", *x);
        free(x);
    }

    remove_all(&l);
}

void graph_demo() {
    int *n1_visited = malloc(sizeof(int));
    *n1_visited = 0;
    int *n2_visited = malloc(sizeof(int));
    *n2_visited = 0;
    int *n3_visited = malloc(sizeof(int));
    *n3_visited = 0;
    int *n4_visited = malloc(sizeof(int));
    *n4_visited = 0;

    graph *g = create_graph();
    node *n1 = create_node(1, 2, 3, 4, 5, 6, n1_visited);
    node *n2 = create_node(4, 5, 6, 6, 7, 7, n2_visited);
    node *n3 = create_node(3, 2, 4, 1, 2, 4, n3_visited);
    node *n4 = create_node(9, 10, 2, 3, 2, 3, n4_visited);
    edge *e1 = create_edge(n1, n2, 1, 2, 3, 4);
    edge *e2 = create_edge(n2, n3, 1, 2, 3, 4);
    edge *e3 = create_edge(n1, n4, 1, 2, 3, 5);
    edge *e4 = create_edge(n2, n4, 1, 2, 3, 4);

    add_node(g, n1);
    add_node(g, n2);
    add_node(g, n3);
    add_node(g, n4);

    add_edge(g, e1);
    add_edge(g, e2);
    add_edge(g, e3);
    add_edge(g, e4);

    print_graph(g, 0);

    printf("Start dfs:\n");
    dfs(g, n1);

    printf("After dfs:\n");
    print_graph(g, 0);

    printf("After remove:\n");
    remove_edge(g, e2);
    remove_node(g, n4);
    print_graph(g, 0);

    destroy_graph(g);
}

int main() {
    graph_demo();

    return 0;
}
