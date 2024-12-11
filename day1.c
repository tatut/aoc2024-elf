#include "aoc.h"
#include <stdlib.h>

int comp(const void *a, const void *b) { return *(long *)a - *(long *)b; }

aoc_main({
    size_t len;
    char* in = input("day1.txt", &len);
    size_t count = lines_count(in);

    long *left = malloc(sizeof(long)*count);
    long *right = malloc(sizeof(long)*count);

    size_t i=0;
    lines_each(in, line, {
        char *after;
        left[i] = number(line, &after);
        right[i] = number(after, &after);
        i++;
      });

    qsort(left, count, sizeof(long), comp);
    qsort(right, count, sizeof(long), comp);

    long p1 = 0;
    for(size_t i =0; i < count; i++) {
      p1 += abs((int)left[i] - (int)right[i]);
    }
    long p2 = 0;
    for(size_t l=0;l<count;l++){
      long occurs=0;
      for(size_t r=0;r<count;r++){
        if(left[l]==right[r]) occurs++;
      }
      p2 += left[l]*occurs;
    }

    printf("Part1: %ld\nPart2: %ld\n", p1, p2);
  });
