//
// Created by cyx on 2021/12/18.
//

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include "graph.h"

#define ARROWLENGTH 10

static void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if ((key == GLFW_KEY_ESCAPE || key == GLFW_KEY_ENTER) && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GLFW_TRUE);
}bz

GLFWwindow *create_window() {
    const char* description;
    if (!glfwInit())
    {
        glfwGetError(&description);
        printf("Error: %s\n", description);
        exit(EXIT_FAILURE);
    }

    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE);
    glfwWindowHint(GLFW_DECORATED, GLFW_FALSE);

    int xpos, ypos, height;
    glfwGetMonitorWorkarea(glfwGetPrimaryMonitor(), &xpos, &ypos, NULL, &height);

    const int size = height / 2;
    GLFWwindow *window = glfwCreateWindow(size, size, "GVL", NULL, NULL);
    if (!window)
    {
        glfwGetError(&description);
        printf("Error: %s\n", description);
        glfwTerminate();
        exit(EXIT_FAILURE);
    }

    glfwSetKeyCallback(window, key_callback);

    glfwSetWindowPos(window, xpos, ypos);
    glfwSetInputMode(window, GLFW_STICKY_KEYS, GLFW_TRUE);

    // set background color
    glfwMakeContextCurrent(window);
    gladLoadGL();
    glClearColor(1.f, 1.f, 1.f, 1.f);

    // set up view
    glViewport(0, 0, size, size);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    // creates a canvas for drawing
    glOrtho(0, size, 0, size, 0.0, 1.0);

    glfwShowWindow(window);

    return window;
}

void draw_line(double v1_x, double v1_y, double v2_x, double v2_y, float line_width, int r, int g, int b) {
    glLineWidth(line_width);
    glColor3f((float)r / 255, (float)g / 255, (float)b / 255);   // TODO
    glBegin(GL_LINES);
    glVertex3f(v1_x, v1_y, 0.0);
    glVertex3f(v2_x, v2_y, 0.0);
    glEnd();
}

void draw_arrow(double v1_x, double v1_y, double v2_x, double v2_y, int line_width, int r, int g, int b) {
    draw_line(v1_x, v1_y, v2_x, v2_y, (float)line_width, r, g, b);

    double length = sqrt((v2_x - v1_x) * (v2_x - v1_x) + (v2_y - v1_y) * (v2_y - v1_y));
    draw_line(v2_x + ((v1_x - (v2_y - v1_y)) - v2_x) * ARROWLENGTH / length,
              v2_y + ((v1_y + (v2_x - v1_x)) - v2_y) * ARROWLENGTH / length,
              v2_x, v2_y, (float)line_width / 2, r, g, b);
    draw_line(v2_x + ((v1_x + (v2_y - v1_y)) - v2_x) * ARROWLENGTH / length,
              v2_y + ((v1_y - (v2_x - v1_x)) - v2_y) * ARROWLENGTH / length,
              v2_x, v2_y, (float)line_width / 2, r, g, b);
}

void draw_circle(double c_x, double c_y, double radius, int r, int g, int b) {
    glColor3f((float)r / 255, (float)g / 255, (float)b / 255);   // TODO
    int num_segments = (int)radius * 360;
    float theta = M_PI * 2 / (float)num_segments;

    glBegin(GL_TRIANGLE_FAN);
    float angle = 0;
    for (int i = 0; i < num_segments; i++)
    {
        double x = radius * cosf(angle);
        double y = radius * sinf(angle);
        glVertex2f(c_x + x, c_y + y);
        angle += theta;
    }
    glEnd();

}

void draw_graph(graph *g) {
    list_node *iter = g->first;
    while(iter) {
        graph_node *gn = (graph_node *)iter->data;
        list_node *edge_iter = gn->edges->first;

        draw_circle(gn->n->x, gn->n->y, gn->n->radius, gn->n->r, gn->n->g, gn->n->b);

        while (edge_iter) {
            edge *e = (edge *)edge_iter->data;
            double length = sqrt((e->end->y - gn->n->y) * (e->end->y - gn->n->y) +
                    (e->end->x - gn->n->x) * (e->end->x - gn->n->x));
            draw_arrow(gn->n->x + (e->end->x - gn->n->x) * gn->n->radius / length,
                    gn->n->y + (e->end->y - gn->n->y) * gn->n->radius / length,
                    e->end->x + (gn->n->x - e->end->x) * gn->n->radius / length,
                    e->end->y + (gn->n->y - e->end->y) * gn->n->radius / length,
                    e->bold, e->r, e->g, e->b);

            edge_iter = edge_iter->next;
        }

        iter = iter->next;
    }
}

int show_graph(graph *g) {
    GLFWwindow *window = create_window();
    while (!glfwWindowShouldClose(window)) {
        glfwMakeContextCurrent(window);
        glClear(GL_COLOR_BUFFER_BIT);

        draw_graph(g);

        glfwSwapBuffers(window);
        glfwWaitEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();

    return 0;
}