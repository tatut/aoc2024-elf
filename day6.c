#include <string.h>
#include <stdbool.h>
#include "aoc.h"

#define FILENAME "day6.txt"

void fwd(int dir, int x, int y, int *nx, int *ny) {
  int cx = x;
  int cy = y;
  switch(dir) {
  case 0: *nx = cx; *ny = cy-1; break; //up
  case 1: *nx = cx+1; *ny = cy; break; //right
  case 2: *nx = cx; *ny = cy+1; break; //down
  case 3: *nx = cx-1; *ny = cy; break; //left
  default: panic("Unknown direction");
  }
}

void walk(struct Grid* g, int x, int y, int dir, bool *visited) {
  while(grid_at(g, x, y)!=0) {
    visited[grid_idx(g, x,y)] = true;
    int nx, ny;
    fwd(dir, x, y, &nx, &ny);
    if(grid_at(g, nx, ny)=='#') dir = (dir + 1) % 4;
    else { x = nx; y = ny; }
  }
}

int count_visited(struct Grid* g, bool *visited) {
  int v = 0;
  grid_each_pos(g, x, y, {
      if(visited[grid_idx(g,x,y)]) v++;
    });
  return v;
}

bool loops(struct Grid *g, int x, int y, int dir, char* visited) {
  while(grid_at(g, x, y)!=0) {
    int nx, ny;
    fwd(dir, x, y, &nx, &ny);
    char at = grid_at(g, nx, ny);
    if(at=='#') {
      char visits = visited[grid_idx(g,x,y)];
      if(visits & (1 << dir)) return true;
      visited[grid_idx(g,x,y)] = visits | (1 << dir);
      dir = (dir + 1) % 4; }
    else { x = nx; y = ny; }
  }
  return false;
}

int main(int argc, char** argv) {
  time_start();
  struct Grid* g = grid(FILENAME);
  int startx, starty;
  grid_find(g, '^', startx, starty);

  // allocate grid sized array to track visited
  bool* visited = calloc(grid_size(g), sizeof(bool));
  walk(g, startx, starty, 0, visited);
  int part1 = count_visited(g, visited);

  // use each visited and make it an obstruction
  int part2 = 0;
  char* visited2 = malloc(grid_size(g)*sizeof(char));
  grid_set(g, startx, starty, '.');
  grid_each_pos(g, vx, vy, {
      if(visited[grid_idx(g,vx,vy)] && (vx != startx || vy != starty)) {
        grid_set(g, vx, vy, '#');
        memset(visited2, 0, grid_size(g)*sizeof(char));
        if(loops(g, startx, starty, 0, visited2)) part2++;
        grid_set(g, vx, vy, '.');
      }
    });
  printf("Part1: %d, Part2: %d\n", part1, part2);
  time_end();
}
