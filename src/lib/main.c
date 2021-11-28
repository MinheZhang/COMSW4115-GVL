#include <stdio.h>
#include "list.h"

int main() {
    list l = {NULL, NULL, sizeof(int)};
    for (int i = 0; i < 10; ++i) {
        int *t = malloc(sizeof(i));
        *t = i;
        insert_back(t, &l);
    }

    for (int i = 0; i < 5; ++i) {
        int *x = (int *)remove_front(&l);
        printf("%d\n", *x);
        free(x);
    }

    remove_all(&l);

    return 0;
}
