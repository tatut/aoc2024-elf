#include "aoc.h"

#define FILENAME "day10.txt"

int score2(struct Grid* g, int x, int y, char ch) {
  if(ch == 57) return 1;
  int s = 0;
  grid_neighbors(g, x,y, nx,ny, val, {
      if(val == ch + 1) s += score2(g, nx, ny, val);
    });
  return s;
}

void score1(struct Grid *g, int *founds, int id, int x, int y, char ch) {
  if(ch == 57) { founds[y*g->w + x] = id; return; }
  grid_neighbors(g, x,y, nx,ny, val, {
      if(val == ch + 1) score1(g, founds, id, nx, ny, val);
    });
}

int count_score1(struct Grid *g, int *founds, int id) {
  int score=0;
  for(size_t x=0;x<g->w;x++) {
    for(size_t y=0;y<g->h;y++) {
      if(founds[y*g->w + x] == id) score++;
    }
  }
  return score;
}

int main(int argc, char **argv) {
  time_start();
  struct Grid* g = grid(FILENAME);

  // create a grid sized array to keep track during what
  // iteration a given 9 was found
  int part1 = 0;
  int part2 = 0;

  int* founds = (int*) calloc(g->w*g->h, sizeof(int));
  int id=0; // iter id
  grid_each(g, x, y, val, {
      if(val == 48) {
        id++;
        score1(g, founds, id, x, y, val);
        part1 += count_score1(g, founds, id);
        part2 += score2(g, x, y, val);
      }
    });

  printf("Part1: %d\nPart2: %d\n", part1, part2);

  time_end();
}
